use std::convert::TryFrom;
use std::io::Write;

use bytes::Bytes;
use ckb_vm::memory::{round_page_up, FLAG_EXECUTABLE, FLAG_WRITABLE};
use ckb_vm::registers::{A0, SP};
use ckb_vm::{CoreMachine, DefaultCoreMachine, DefaultMachine, Memory, SparseMemory, WXorXMemory};

use crate::frontend::FrontEnd;
use crate::frontend::FrontEndState;
use crate::graphviz::IrGraphvizFile;
use crate::riscv_backend;
use crate::riscv_backend::ui_immediate;
use crate::riscv_backend::EnableImmediateIntegers;
use crate::riscv_backend::Executable;
use crate::riscv_backend::{DumpCode, RelocationKind};
use crate::riscv_simulator;
use crate::riscv_simulator::DumpState;
use crate::utils::{stderr, typecheck};

struct BackEndPermutation(usize);

impl BackEndPermutation {
    const PARAMETER_COUNT: usize = 1;
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

#[test]
fn proper_spilling() {
    test_backend(
        function_name!().to_owned(),
        "
        let a = 1
        let b = 2
        (a + 3) * (4 + 5)
        (a + 6) / (7 * 8 + 9)
        (a + 10) * 11 * (b - 12) / 13
        (14 / 15) * (16 - 17)
        (a + 18) * (b + 19)
        ",
        399,
    );
}

#[test]
fn branching() {
    test_backend(
        function_name!().to_owned(),
        "
        let a = 1
        let b = 2
        if a {
            if (b) 3 else 4
        } else {
            if (b) 5 else 6
        }
        ",
        3,
    );
}

#[test]
fn functions() {
    test_backend(
        function_name!().to_owned(),
        "
        let f = \\ (a: num) -> a + 1
        let g = \\ (h: \\ (a: num) -> num) -> h 2
        g f
        ",
        3,
    );
}

#[test]
fn function_return() {
    test_backend(
        function_name!().to_owned(),
        "
        let f = \\ (x: num) -> x + 1
        let a = 10 + 1
        f (a * 2) + f (a / 2) - f (a + 2)
        // 11 * 2 + 1 + 11 / 2 + 1 - 11 - 2 - 1
        // 22 + 1 + 5 - 13 = 15
        ",
        15,
    );
}

#[test]
fn misaligned_fetch() {
    test_backend(
        function_name!().to_owned(),
        "
        fn f (a: num, b: num) a + b
        fn g (f: \\ (a: num, b: num) -> num, a: num, b: num) f a b + 1

        fn h (f: \\ (a: num, b: num) -> num, g: \\ (f: \\ (a: num, b: num) -> num, a: num, b: num) -> num) {
            let a = 1
            let b = 2

            g f a b
            g f 1 2
            g f 1 2
            g f 1 2
            g f 1 2
            g f 1 2
            g f 1 2
            g f 1 2
        }

        h f g
        ",
        4,
    );
}

fn test_backend(source_name: String, source_code: &str, expected_result: u32) {
    let code = typecheck(source_name.clone(), source_code).unwrap();
    let mut state = FrontEndState::new();
    let module = FrontEnd::new(&source_name, &mut state)
        .enable_warnings(false)
        .include_comments(true)
        .enable_cfp(false)
        .enable_dce(false)
        .build(&code);
    if crate::test_config::EMIT_MODULE_GRAPHVIZ_FILE {
        let file =
            IrGraphvizFile::create(format!("riscv_backend_{}_cfg.dot", source_name)).unwrap();
        file.write(&module).unwrap();
    }

    let stderr = &mut stderr();
    let dump_code = false;
    if dump_code {
        writeln!(stderr).unwrap();
    }

    let rvsim_backend = Rvsim;
    let ckbvm_backend = Ckbvm;
    let backends: &[&dyn VmBackend] = &[&rvsim_backend, &ckbvm_backend];
    let mut backend_permutation = BackEndPermutation::new();
    while !backend_permutation.is_empty() {
        let enable_immediate_integers =
            EnableImmediateIntegers(backend_permutation.enable_immediate_integers());
        let dump_code = if dump_code {
            DumpCode::Yes(stderr)
        } else {
            DumpCode::No
        };
        let image = riscv_backend::generate(&module, dump_code, enable_immediate_integers).unwrap();

        for &backend in backends {
            let result = backend.run(&image);
            assert_eq!(
                result,
                expected_result,
                "\nin VM backend: {}",
                backend.name()
            );
        }

        backend_permutation.next();
    }
}

trait VmBackend {
    fn name(&self) -> &'static str {
        core::any::type_name::<Self>()
    }

    fn run(&self, image: &Executable) -> u32;
}

struct Rvsim;

impl VmBackend for Rvsim {
    fn run(&self, image: &Executable) -> u32 {
        let simulator = riscv_simulator::run(&image, DumpState::None).unwrap();
        simulator.cpu.x[A0 as usize]
    }
}

struct Ckbvm;

impl VmBackend for Ckbvm {
    fn run(&self, image: &Executable) -> u32 {
        type Register = u32;

        let mut machine = DefaultMachine::<
            DefaultCoreMachine<Register, WXorXMemory<Register, SparseMemory<Register>>>,
        >::default();

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

        const DATA_START_ADDRESS: u32 = 0x0010_0000;
        const STACK_SIZE: usize = 1024;
        let total_data_size = image.data.len() + STACK_SIZE;
        machine
            .memory_mut()
            .init_pages(
                u64::from(DATA_START_ADDRESS),
                round_page_up(u64::try_from(total_data_size).unwrap()),
                FLAG_WRITABLE,
                Some(Bytes::from(image.data.as_slice())),
                0,
            )
            .unwrap();

        for relocation in &image.relocations {
            let target_offset = match relocation.kind {
                RelocationKind::Function => CODE_START_ADDRESS,
                RelocationKind::DataLoad | RelocationKind::DataStore => DATA_START_ADDRESS,
            };
            let target = i32::try_from(relocation.target + target_offset).unwrap();
            let immediate = ui_immediate(target).unwrap();
            let code_memory = machine.memory_mut().inner_mut();

            let u_instruction_offset = CODE_START_ADDRESS + relocation.offset;
            let next_instruction_offset = u_instruction_offset + 4;
            let mut u_instruction = code_memory.load32(&u_instruction_offset).unwrap();
            u_instruction = u_instruction & 0x0000_0FFF | immediate.upper as u32;
            code_memory
                .store32(&u_instruction_offset, &u_instruction)
                .unwrap();

            let next_instruction = code_memory.load32(&next_instruction_offset).unwrap();
            let next_instruction = match relocation.kind {
                RelocationKind::Function | RelocationKind::DataLoad => {
                    next_instruction & 0x000F_FFFF | ((immediate.lower as u32) << 20)
                }
                RelocationKind::DataStore => {
                    const LOW_MASK: u32 = 0b11111;
                    const LOW_OFFSET: u32 = 7;
                    const HIGH_MASK: u32 = 0b111_1111;
                    const HIGH_OFFSET: u32 = 25;
                    next_instruction & !(LOW_MASK << LOW_OFFSET | HIGH_MASK << HIGH_OFFSET)
                        | ((immediate.lower as u32 & LOW_MASK) << LOW_OFFSET)
                        | (((immediate.lower >> 5) as u32 & HIGH_MASK) << HIGH_OFFSET)
                }
            };
            code_memory
                .store32(&next_instruction_offset, &next_instruction)
                .unwrap();
        }

        machine.set_pc(image.entry);
        machine.set_register(
            SP,
            DATA_START_ADDRESS + u32::try_from(total_data_size).unwrap(),
        );

        machine.run().unwrap();
        machine.registers()[A0]
    }
}
