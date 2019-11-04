use crate::frontend::FrontEnd;
use crate::frontend::FrontEndState;
use crate::graphviz::IrGraphvizFile;
use crate::ir::eval::eval;
use crate::ir::IRModule;
use crate::ir::Value;
use crate::utils::typecheck;
use std::ffi::OsStr;
use std::iter::{once, Chain, Once};
use std::path::Path;
use std::rc::Rc;

#[test]
fn generic() {
    test_frontend(
        location!(),
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
        None,
        |module| assert_eq!(eval(&module), Value::String(Rc::new("(hello; world)<bye, world; seeya>".to_owned())))
    );
}

#[test]
fn block_removal() {
    test_frontend(
        location!(),
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
        None,
        |module| assert_eq!(eval(&module), Value::Int(3)),
    );
}

#[test]
fn block_removal2() {
    test_frontend(
        location!(),
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
        "
        .repeat(7),
        None,
        |module| assert_eq!(eval(&module), Value::Unit),
    );
}

#[test]
fn marker_eval() {
    test_frontend(
        location!(),
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
        None,
        |module| assert_eq!(eval(&module), Value::Int(10)),
    );
}

#[test]
fn conditional_cfp() {
    test_frontend(
        location!(),
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
        Some(false),
        |module| assert_eq!(module.cfg.edge_count(), 0),
    );
}

fn test_frontend(
    source_name: String,
    source_code: &str,
    forbidden_cfp_variant: Option<bool>,
    check: fn(IRModule),
) {
    let code = typecheck(source_name, source_code).unwrap();
    for config in frontend_config_permutations(forbidden_cfp_variant) {
        let mut state = FrontEndState::new();
        let module = FrontEnd::new(&location!(), &mut state)
            .enable_warnings(false)
            .include_comments(config.include_comments)
            .enable_cfp(config.enable_cfp)
            .enable_dce(config.enable_dce)
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
                "frontend_{}_{}_cfg.dot",
                test_src_file_name,
                line!()
            ))
            .unwrap();
            file.write(&module).unwrap();
        }

        check(module);
    }
}

struct FrontendConfig {
    include_comments: bool,
    enable_cfp: bool,
    enable_dce: bool,
}

fn frontend_config_permutations(
    forbidden_cfp_variant: Option<bool>,
) -> impl Iterator<Item = FrontendConfig> {
    bool_variants()
        .flat_map(move |include_comments| {
            bool_variants().flat_map(move |enable_cfp| {
                bool_variants().map(move |enable_dce| FrontendConfig {
                    include_comments,
                    enable_cfp,
                    enable_dce,
                })
            })
        })
        .filter(move |config| {
            forbidden_cfp_variant
                .filter(|enable_cfp| *enable_cfp == config.enable_cfp)
                .is_none()
        })
}

fn bool_variants() -> Chain<Once<bool>, Once<bool>> {
    once(false).chain(once(true))
}
