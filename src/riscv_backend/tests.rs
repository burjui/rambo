use std::convert::TryFrom;
use std::ffi::OsStr;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use termcolor::{Color, ColorSpec, WriteColor};

use crate::frontend::FrontEnd;
use crate::graphviz::graphviz_dot_write;
use crate::riscv_backend;
use crate::riscv_backend::DumpCode;
use crate::riscv_backend::registers;
use crate::riscv_backend::RICSVImage;
use crate::riscv_decoder::decode;
use crate::riscv_simulator;
use crate::riscv_simulator::DumpState;
use crate::utils::GenericResult;
use crate::utils::stderr;

macro_rules! test_backend{
    ($name: ident, $code: expr, $check: expr) => {
        #[test]
        fn $name() -> GenericResult<()> {
            let code = typecheck!($code)?;
            let cfg = FrontEnd::new(&location!())
                .enable_warnings(false)
                .include_comments(true)
                .enable_cfp(false)
                .enable_dce(false)
                .build(&code);
            let test_src_path = Path::new(file!());
            let test_src_file_name = test_src_path
                .file_name()
                .and_then(OsStr::to_str)
                .expect(&format!("failed to extract the file name from path: {}", test_src_path.display()));
            let mut file = File::create(format!("riscv_backend_{}_{}_cfg.dot", test_src_file_name, line!()))?;
            graphviz_dot_write(&mut file, &cfg)?;

            let stderr = &mut stderr();
            writeln!(stderr)?;
            let dump_code = false;
            let image = riscv_backend::generate(&cfg, DumpCode(dump_code))?;
            if dump_code {
                writeln!(stderr)?;
                writeln!(stderr, "----- CODE DUMP -----")?;
                for (op, offset) in decode(&image.code) {
                    let offset = u32::try_from(offset)?;
                    if let Some(comments) = image.comments.get(&offset) {
                        for comment in comments {
                            writeln!(stderr, "// {}", comment)?;
                        }
                    }
                    stderr.set_color(ColorSpec::new()
                        .set_fg(Some(Color::Black))
                        .set_intense(true))?;
                    writeln!(stderr, "[0x{:08x}]  {:?}", image.code_base_address + u32::try_from(offset)?, op)?;
                    stderr.reset()?;
                }
                writeln!(stderr, "---------------------")?;
            }
            let check: fn(RICSVImage) -> GenericResult<()> = $check;
            check(image)
        }
    }
}

test_backend!{
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
        assert_eq!(simulator.cpu.x[registers::RESULT as usize], 399);
        Ok(())
    }
}

test_backend!{
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
        assert_eq!(simulator.cpu.x[registers::RESULT as usize], 3);
        Ok(())
    }
}
