use crate::frontend::FrontEnd;
use crate::frontend::FrontEndState;
use crate::graphviz::IrGraphvizFile;
use crate::riscv_backend;
use crate::riscv_backend::DumpCode;
use crate::riscv_backend::EnableImmediateIntegers;
use crate::riscv_exe::run;
use crate::riscv_exe::DumpState;
use crate::utils::function_name;
use crate::utils::{stderr, typecheck};
use riscv::A0;
use riscv_backend::EnableComments;
use std::io::Write;

struct BackEndPermutation(usize);

impl BackEndPermutation {
    const PARAMETER_COUNT: usize = 2;
    const PERMUTATION_COUNT: usize = 1 << Self::PARAMETER_COUNT;

    const fn new() -> Self {
        Self(0)
    }

    const fn is_empty(&self) -> bool {
        self.0 >= Self::PERMUTATION_COUNT
    }

    fn next(&mut self) {
        self.0 += 1;
    }

    const fn enable_immediate_integers(&self) -> bool {
        self.parameter(0)
    }

    const fn enable_comments(&self) -> bool {
        self.parameter(0)
    }

    const fn parameter(&self, index: usize) -> bool {
        self.0 & (1 << index) != 0
    }
}

#[test]
fn proper_spilling() {
    test_backend(
        function_name!(),
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
        function_name!(),
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
        function_name!(),
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
        function_name!(),
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
fn misaligned_fetch_bug() {
    test_backend(
        function_name!(),
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

fn test_backend(source_name: &str, source_code: &str, expected_result: i64) {
    let code = typecheck(source_name.to_owned(), source_code).unwrap();
    let mut state = FrontEndState::new();
    let module = FrontEnd::new(source_name, &mut state)
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

    let mut backend_permutation = BackEndPermutation::new();
    while !backend_permutation.is_empty() {
        let enable_immediate_integers =
            EnableImmediateIntegers(backend_permutation.enable_immediate_integers());
        let enable_comments = EnableComments(backend_permutation.enable_comments());
        let dump_code = if dump_code {
            DumpCode::Yes(stderr)
        } else {
            DumpCode::No
        };
        let image = riscv_backend::generate(
            &module,
            dump_code,
            enable_immediate_integers,
            enable_comments,
        )
        .unwrap();

        let result = run(&image, DumpState::None).unwrap().read_register(A0);
        assert_eq!(result, expected_result);

        backend_permutation.next();
    }
}
