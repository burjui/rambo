use std::collections::HashSet;
use std::ffi::OsStr;
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::rc::Rc;

use crate::frontend::FrontEnd;
use crate::frontend::FrontEndState;
use crate::graphviz::graphviz_dot_write_cfg;
use crate::ir::IRModule;
use crate::ir::Value;
use crate::utils::TestResult;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum ForbiddenPermutation {
    IncludeComments(bool),
    EnableCfp(bool),
    EnableDce(bool),
}

struct FrontEndPermutation(usize);

impl FrontEndPermutation {
    const PARAMETER_COUNT: usize = 3;
    const PERMUTATION_COUNT: usize = 1 << Self::PARAMETER_COUNT;

    fn new() -> Self {
        Self(0)
    }

    fn is_empty(&self) -> bool {
        self.0 >= Self::PERMUTATION_COUNT
    }

    fn is_forbidden(&self, forbidden_permutations: &HashSet<ForbiddenPermutation>) -> bool {
        forbidden_permutations.contains(&ForbiddenPermutation::IncludeComments(
            self.include_comments(),
        )) || forbidden_permutations.contains(&ForbiddenPermutation::EnableCfp(self.enable_cfp()))
            || forbidden_permutations.contains(&ForbiddenPermutation::EnableDce(self.enable_dce()))
    }

    fn next(&mut self) {
        self.0 += 1;
    }

    fn include_comments(&self) -> bool {
        self.parameter(0)
    }

    fn enable_cfp(&self) -> bool {
        self.parameter(1)
    }

    fn enable_dce(&self) -> bool {
        self.parameter(2)
    }

    fn parameter(&self, index: usize) -> bool {
        self.0 & (1 << index) != 0
    }
}

#[cfg(test)]
impl fmt::Debug for FrontEndPermutation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "FrontEndPermutation {{\
             \n  include_comments: {},\
             \n  enable_cfp: {},\
             \n  enable_dce: {},\
             \n}}",
            self.include_comments(),
            self.enable_cfp(),
            self.enable_dce()
        )
    }
}

macro_rules! test_frontend {
    ($name: ident, $code: expr, $check: expr $(, $forbidden_permutation: expr)*) => {
        #[test]
        fn $name() -> TestResult {
            use std::iter::FromIterator;

            let code = typecheck!($code)?;
            let forbidden_permutations = HashSet::from_iter(vec![ $($forbidden_permutation,)* ]);
            let mut frontend_permutation = FrontEndPermutation::new();
            while !frontend_permutation.is_empty() {
                if frontend_permutation.is_forbidden(&forbidden_permutations) {
                    frontend_permutation.next();
                    continue;
                }

                let mut state = FrontEndState::new();
                let module = FrontEnd::new(&location!(), &mut state)
                    .enable_warnings(false)
                    .include_comments(frontend_permutation.include_comments())
                    .enable_cfp(frontend_permutation.enable_cfp())
                    .enable_dce(frontend_permutation.enable_dce())
                    .build(&code);
                let test_src_path = Path::new(file!());
                let test_src_file_name = test_src_path
                    .file_name()
                    .and_then(OsStr::to_str)
                    .expect(&format!("failed to extract the file name from path: {}", test_src_path.display()));
                let mut file = File::create(format!("frontend_{}_{}_cfg.dot", test_src_file_name, line!()))?;
                graphviz_dot_write_cfg(&mut file, &module)?;
                let check: fn(IRModule) = $check;
                check(module);

                frontend_permutation.next();
            }
            Ok(())
        }
    }
}

macro_rules! test_frontend_eval {
    ($name: ident, $code: expr, $expected_result: expr $(, $forbidden_permutation: expr)*) => {
        test_frontend!{ $name, $code,
            |module| {
                let value = crate::ir::eval::EvalContext::new(&module, &module.functions).eval();
                assert_eq!(value, $expected_result);
            }
            $(, $forbidden_permutation)*
        }
    }
}

test_frontend_eval! {
    generic,
    "
    let x = 47
    let y = 29
    let z = y
    1
    let r = if 0 {
        let a = z
        let b = a
        z = 22
        x + b + y
        1
    } else {
        z = 33
        1
    }
    z
    r + 1
    z + 1

    z = 6
    z + 7
    z = 8
    z + 9

    let f = λ (g: λ (u:str, v:str) -> str, u:str, v:str) -> g u v
    let s1 = f (λ (u:str, v:str) -> \"(\" + u + \"; \" + v + \")\") \"hello\" \"world\"
    let s2 = f (λ (u:str, v:str) -> \"<\" + u + \"; \" + v + \">\") (\"bye, \" + \"world\") \"seeya\"
    let result = s1 + s2
    result
    ",
    Value::String(Rc::new("(hello; world)<bye, world; seeya>".to_owned()))
}

test_frontend_eval! {
    block_removal,
    "
    let x = 0
    let y = 999
    if x {
        x = 1
    } else {
        x = 2
    }
    if y {
        x = 3
    } else {
        x = 4
    }
    x
    ",
    Value::Int(3)
}

test_frontend_eval! {
    block_removal2,
    &"
    let x = \"abc\"
    let y = \"efg\"
    let z = λ (a:str, b:str) -> {
        let result = a + b
        result
    }
    if 0 {
        \"false\"
    }
    else {
        let a = (z x y)
        z a \"/test\"
    }
    let nn = 1
    ".repeat(7),
    Value::Unit
}

test_frontend_eval! {
    marker_eval,
    "
    let a = 1
    let b = 2
    (a + 1) * (b - 1)
    let f = λ (a: num, b: num) -> a + b
    let g = λ (f: λ (a: num, b: num) -> num, a: num, b: num) → f a b + 1
    g f 1 2
    (a + 1) * (b - 1)
    a = 10
    g f 1 2
    (a + 1) * (b - 1)
    a = 10
    ",
    Value::Int(10)
}

test_frontend! {
    conditional_cfp,
    "
    let f = λ (a: num, b: num) -> a + b
    let x = 0
    if x {
        x = f 3 4
    } else {
        x = f 5 6
    }
    x
    ",
    |module| assert_eq!(module.cfg.edge_count(), 0),
    ForbiddenPermutation::EnableCfp(false)
}
