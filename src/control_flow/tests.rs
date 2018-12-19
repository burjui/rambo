use std::error::Error;
use std::fs::File;

use crate::codegen::generate_ssa;
use crate::control_flow::build_control_flow_graph;
use crate::graphviz::Graphviz;

type TestResult = Result<(), Box<dyn Error>>;

#[test]
fn control_flow_graph_builder() -> TestResult {
    let code = "
        let x = \"abc\"
        let y = \"efg\"
        let z = \\ (a:str, b:str) -> {
            if 1 {
                a + \"--\"
            } else {
                \"?\"
            } + b
        }
        z \"foo\" \"bar\"
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
        let c = \\ (a:num, b:num) -> 4
        c 1 2
        a1 = 10
        (0 - a1) * (0 - 1)
        (a1 + a1)
        (a1 - a1)
        s
    ";
    let ssa = generate_ssa(&typecheck!(code)?);
    let graph = build_control_flow_graph(&ssa);
    Graphviz::new().include_comments(false).fmt(&mut File::create("cfg.dot")?, &graph)?;
    Ok(())
}
