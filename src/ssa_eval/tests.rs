use std::error::Error;

use crate::codegen::Codegen;
use crate::ssa_eval::SSAEvaluator;

type TestResult = Result<(), Box<dyn Error>>;

#[test]
fn eval() -> TestResult {
    let code = "
    let x = \"abc\"
    let y = \"efg\"
    let z = λ (a:str, b:str) -> {
        if 1 {
            a + \"--\"
        } else {
            \"?\"
        } + b
    }
    let s = if 0 {
        \"false\"
        y = \"x\"
        x = \"z\"
    } else {
        let a = (z x y)
        z a \"/test\"
    }
    let nn = 1
    let a1 = 0
    let a2 = a1
    let a3 = a1 + a2
    let c = λ (a:num, b:num) -> 4
    c 1 2
    a1 = 10
    (0 - a1) * (0 - 1)
    (a1 + a1)
    (a1 - a1)
    s
    ";
    let ssa = Codegen::new().build(&typecheck!(code)?);
    let result = SSAEvaluator::new().eval(&ssa);
    let (s_address, s_offset) = result.value.str();
    let s = SSAEvaluator::str(&result.ram, s_address, s_offset);
    assert_eq!(s, "abc--efg--/test");
    Ok(())
}
