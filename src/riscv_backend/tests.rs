use crate::frontend::FrontEnd;
use crate::frontend::FrontEndState;
use crate::graphviz::IrGraphvizFile;
use crate::riscv_backend;
use crate::riscv_backend::DumpCode;
use crate::riscv_backend::EnableImmediateIntegers;
use crate::riscv_backend::RICSVImage;
use crate::riscv_backend::{registers, ui_immediate};
use crate::riscv_simulator;
use crate::riscv_simulator::DumpState;
use crate::utils::stderr;
use crate::utils::GenericResult;
use bytes::Bytes;
use ckb_vm::memory::{round_page_up, FLAG_EXECUTABLE, FLAG_WRITABLE};
use ckb_vm::registers::{A0, SP};
use ckb_vm::{CoreMachine, DefaultCoreMachine, DefaultMachine, FlatMemory, Memory};
use std::convert::TryFrom;
use std::ffi::OsStr;
use std::io::Write;
use std::path::Path;

struct BackEndPermutation(usize);

impl BackEndPermutation {
    const PARAMETER_COUNT: usize = 3;
    const PERMUTATION_COUNT: usize = 1 << Self::PARAMETER_COUNT;

    fn new() -> Self {
        Self(0)
    }

    fn is_empty(&self) -> bool {
        self.0 >= Self::PERMUTATION_COUNT
    }

    fn next(&mut self) {
        self.0 += 1;
    }

    fn enable_immediate_integers(&self) -> bool {
        self.parameter(0)
    }

    fn parameter(&self, index: usize) -> bool {
        self.0 & (1 << index) != 0
    }
}

macro_rules! test_backend {
    ($name: ident, $code: expr, $check: expr) => {
        #[test]
        fn $name() -> GenericResult<()> {
            let code = typecheck!($code)?;
            let mut state = FrontEndState::new();
            let module = FrontEnd::new(&location!(), &mut state)
                .enable_warnings(false)
                .include_comments(true)
                .enable_cfp(false)
                .enable_dce(false)
                .build(&code);
            if crate::test_config::EMIT_MODULE_GRAPHVIZ_FILE {
                let test_src_path = Path::new(file!());
                let test_src_file_name =
                    test_src_path
                        .file_name()
                        .and_then(OsStr::to_str)
                        .expect(&format!(
                            "failed to extract the file name from path: {}",
                            test_src_path.display()
                        ));
                let file = IrGraphvizFile::create(format!(
                    "riscv_backend_{}_{}_cfg.dot",
                    test_src_file_name,
                    line!()
                ))?;
                file.write(&module)?;
            }

            let stderr = &mut stderr();
            let dump_code = DumpCode(false);
            if *dump_code {
                writeln!(stderr)?;
            }

            let mut backend_permutation = BackEndPermutation::new();
            while !backend_permutation.is_empty() {
                let enable_immediate_integers =
                    EnableImmediateIntegers(backend_permutation.enable_immediate_integers());
                let image = riscv_backend::generate(&module, dump_code, enable_immediate_integers)?;
                let check: fn(RICSVImage) -> GenericResult<()> = $check;
                check(image)?;
                backend_permutation.next();
            }
            Ok(())
        }
    };
}

test_backend! {
    proper_spilling,
    "
    let a = 1
    let b = 2
    (a + 3) * (4 + 5)
    (a + 6) / (7 * 8 + 9)
    (a + 10) * 11 * (b - 12) / 13
    (14 / 15) * (16 - 17)
    (a + 18) * (b + 19)
    ",
    |image| {
        let simulator = riscv_simulator::run(&image, DumpState::None)?;
        assert_eq!(399, simulator.cpu.x[registers::A0 as usize]);
        Ok(())
    }
}

test_backend! {
    branching,
    "
    let a = 1
    let b = 2
    if a {
        if (b) 3 else 4
    } else {
        if (b) 5 else 6
    }
    ",
    |image| {
        let simulator = riscv_simulator::run(&image, DumpState::None)?;
        assert_eq!(3, simulator.cpu.x[registers::A0 as usize]);
        Ok(())
    }
}

test_backend! {
    functions,
    "
    let f = \\ (a: num) -> a + 1
    let g = \\ (h: \\ (a: num) -> num) -> h 2
    g f
    ",
    |image| {
        let simulator = riscv_simulator::run(&image, DumpState::None)?;
        assert_eq!(3, simulator.cpu.x[registers::A0 as usize]);
        Ok(())
    }
}

test_backend! {
    function_return,
    "
    let f = \\ (x: num) -> x + 1
    let a = 10 + 1
    f (a * 2) + f (a / 2) - f (a + 2)
    // 11 * 2 + 1 + 11 / 2 + 1 - 11 - 2 - 1
    // 22 + 1 + 5 - 13 = 15
    ",
    |image| {
        let simulator = riscv_simulator::run(&image, DumpState::None)?;
        assert_eq!(15, simulator.cpu.x[registers::A0 as usize]);
        Ok(())
    }
}

// TODO port to riscv_simulator
#[test]
fn ckb() {
    let code = typecheck!(
        "
        let f = \\ (x: num) -> x + 1
        let a = 10 + 1
        f (a * 2) + f (a / 2) - f (a + 2)
        // 11 * 2 + 1 + 11 / 2 + 1 - 11 - 2 - 1
        // 22 + 1 + 5 - 13 = 15
    "
    )
    .unwrap();
    let mut state = FrontEndState::new();
    let module = FrontEnd::new(&location!(), &mut state)
        .enable_warnings(false)
        .include_comments(true)
        .enable_cfp(false)
        .enable_dce(false)
        .build(&code);
    let image =
        riscv_backend::generate(&module, DumpCode(false), EnableImmediateIntegers(true)).unwrap();
    type Register = u32;
    let mut machine =
        DefaultMachine::<DefaultCoreMachine<Register, FlatMemory<Register>>>::default();

    const CODE_START_ADDRESS: u32 = 0x0000_0000;
    machine
        .memory_mut()
        .init_pages(
            u64::from(CODE_START_ADDRESS),
            round_page_up(u64::try_from(image.code.len()).unwrap()),
            FLAG_EXECUTABLE,
            Some(Bytes::from(image.code.as_slice())),
            0,
        )
        .unwrap();

    for relocation in &image.relocations {
        let u_instruction_offset = CODE_START_ADDRESS + relocation.offset;
        let i_instruction_offset = u_instruction_offset + 4;
        let mut u_instruction = machine.memory_mut().load32(&u_instruction_offset).unwrap();
        let mut i_instruction = machine.memory_mut().load32(&i_instruction_offset).unwrap();
        let function_offset =
            i32::try_from(image.function_offsets[&relocation.target] + CODE_START_ADDRESS).unwrap();
        let immediate = ui_immediate(function_offset).unwrap();
        u_instruction = u_instruction & 0x0000_0FFF | immediate.upper as u32;
        i_instruction = i_instruction & 0x000F_FFFF | ((immediate.lower as u32) << 20);
        machine
            .memory_mut()
            .store32(&u_instruction_offset, &u_instruction)
            .unwrap();
        machine
            .memory_mut()
            .store32(&i_instruction_offset, &i_instruction)
            .unwrap();
    }

    const DATA_START_ADDRESS: u64 = 0x0010_0000;
    const STACK_SIZE: usize = 1024;
    let total_data_size = image.data.len() + STACK_SIZE;
    machine
        .memory_mut()
        .init_pages(
            DATA_START_ADDRESS,
            round_page_up(u64::try_from(total_data_size).unwrap()),
            FLAG_WRITABLE,
            Some(Bytes::from(image.data.as_slice())),
            0,
        )
        .unwrap();

    machine.set_pc(image.entry);
    machine.set_register(
        SP,
        u32::try_from(DATA_START_ADDRESS + u64::try_from(total_data_size).unwrap()).unwrap(),
    );

    machine.run().unwrap();
    assert_eq!(machine.registers()[A0], 15);
}
