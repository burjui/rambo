use std::{
    iter::{once, Chain, Once},
    rc::Rc,
};

use crate::{
    frontend::{FrontEnd, FrontEndState},
    graphviz::IrGraphvizFile,
    ir::{eval::eval, FunctionMap, IRModule, Value},
    utils::typecheck,
};

#[test]
fn generic() {
    test_frontend(
        function_name!().to_owned(),
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
        ForbiddenPermutations::none(),
        |module| {
            assert_eq!(
                eval(&module),
                Value::String(Rc::new("(hello; world)<bye, world; seeya>".to_owned()))
            )
        },
    );
}

#[test]
fn block_removal1() {
    test_frontend(
        function_name!().to_owned(),
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
        ForbiddenPermutations::none(),
        |module| assert_eq!(eval(&module), Value::Int(3)),
    );
}

#[test]
fn block_removal2() {
    test_frontend(
        function_name!().to_owned(),
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
        ForbiddenPermutations::none(),
        |module| assert_eq!(eval(&module), Value::Unit),
    );
}

#[test]
fn marker_eval() {
    test_frontend(
        function_name!().to_owned(),
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
        ForbiddenPermutations::none(),
        |module| assert_eq!(eval(&module), Value::Int(10)),
    );
}

#[test]
fn conditional_cfp() {
    test_frontend(
        function_name!().to_owned(),
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
        ForbiddenPermutations::none()
            .enable_cfp(false)
            .enable_dce(true),
        |module| {
            assert_eq!(
                find_main(&module.functions)
                    .unwrap()
                    .cfg
                    .edge_indices()
                    .count(),
                0
            );
        },
    );
}

#[test]
fn not_stealing_definitions_from_other_branches() {
    test_frontend(
        function_name!().to_owned(),
        "
        if 1 {
        } else {
        }
        ",
        ForbiddenPermutations::none().enable_cfp(true),
        |module| {
            assert_eq!(
                find_main(&module.functions)
                    .unwrap()
                    .cfg
                    .edge_indices()
                    .count(),
                4
            );
        },
    );
}

fn find_main(function_map: &FunctionMap) -> Option<&IRModule> {
    function_map.values().find(|module| module.name == "main")
}

struct ForbiddenPermutations {
    include_comments: Option<bool>,
    enable_cfp: Option<bool>,
    enable_dce: Option<bool>,
}

impl ForbiddenPermutations {
    const fn none() -> Self {
        Self {
            include_comments: None,
            enable_cfp: None,
            enable_dce: None,
        }
    }

    // fn include_comments(self, include_comments: bool) -> Self {
    //     Self {
    //         include_comments: Some(include_comments),
    //         ..self
    //     }
    // }

    fn enable_cfp(self, enable_cfp: bool) -> Self {
        Self {
            enable_cfp: Some(enable_cfp),
            ..self
        }
    }

    fn enable_dce(self, enable_dce: bool) -> Self {
        Self {
            enable_dce: Some(enable_dce),
            ..self
        }
    }
}

fn test_frontend(
    source_name: String,
    source_code: &str,
    forbidden_permutations: ForbiddenPermutations,
    check: fn(IRModule),
) {
    let code = typecheck(source_name.clone(), source_code).unwrap();
    for config in frontend_config_permutations(forbidden_permutations) {
        let mut state = FrontEndState::new();
        let module = FrontEnd::new(&source_name, &mut state)
            .include_comments(config.include_comments)
            .enable_cfp(config.enable_cfp)
            .enable_inlining(config.enable_inlining)
            .enable_dce(config.enable_dce)
            .build(&code);

        if crate::test_config::EMIT_MODULE_GRAPHVIZ_FILE {
            let file = IrGraphvizFile::create(format!("frontend_{source_name}_cfg.dot")).unwrap();
            file.write(&module).unwrap();
        }

        check(module);
    }
}

struct FrontendConfig {
    include_comments: bool,
    enable_cfp: bool,
    enable_inlining: bool,
    enable_dce: bool,
}

fn frontend_config_permutations(
    forbidden_permutations: ForbiddenPermutations,
) -> impl Iterator<Item = FrontendConfig> {
    bool_variants()
        .flat_map(move |include_comments| {
            bool_variants().flat_map(move |enable_cfp| {
                bool_variants().flat_map(move |enable_inlining| {
                    bool_variants().map(move |enable_dce| FrontendConfig {
                        include_comments,
                        enable_cfp,
                        enable_inlining,
                        enable_dce,
                    })
                })
            })
        })
        .filter(move |config| {
            forbidden_permutations
                .include_comments
                .map_or(true, |it| it != config.include_comments)
                && forbidden_permutations
                    .enable_cfp
                    .map_or(true, |it| it != config.enable_cfp)
                && forbidden_permutations
                    .enable_dce
                    .map_or(true, |it| it != config.enable_dce)
        })
}

fn bool_variants() -> Chain<Once<bool>, Once<bool>> {
    once(false).chain(once(false))
}
