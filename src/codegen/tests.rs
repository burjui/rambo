use num_bigint::BigInt;
use std::error::Error;

use crate::codegen::Codegen;
use crate::codegen::SSAOp;
use crate::codegen::Statement;

#[cfg(feature = "dump_ssa")]
fn dump(code: &str, ssa: &[Statement]) -> TestResult {
    use std::fs::File;
    use std::io::Write;
    use itertools::Itertools;
    use std::sync::Arc;
    use std::sync::Mutex;
    use lazy_static::lazy_static;

    lazy_static! {
        static ref SSA_TXT: Arc<Mutex<File>> = Arc::new(Mutex::new(File::create("ssa.txt").unwrap()));
    }

    let mut file = SSA_TXT.lock().unwrap();
    writeln!(file, "-------------\nCODE:\n{}\n\nSSA:\n{:#?}", code, ssa.iter().format("\n"))?;
    Ok(())
}

macro_rules! match_ssa {
    ($code: expr $(, $patterns: pat)* $(,)? => $($handlers: stmt;)*) => ({
        let ssa = Codegen::new().build(&typecheck!($code)?);
        #[cfg(feature = "dump_ssa")] dump($code, &ssa)?;
        match &ssa as &[Statement] {
            [$($patterns, )*] => Ok({
                $($handlers;)*
            }),
            _ => {
                use itertools::Itertools;
                panic!("match_ssa!()\n\npatterns:\n    {}\n\nssa:\n{:#?}\n\n",
                &[$(stringify!($patterns),)*].iter().format("\n    "),
                ssa.iter().format("\n"))
            }
        }
    });

    ($code: expr, $($patterns: pat),+) => ({match_ssa!($code $(, $patterns)* => ();)})
}

type TestResult = Result<(), Box<dyn Error>>;

#[test]
fn empty() -> TestResult {
    match_ssa!("", Statement { op: SSAOp::Unit, .. })
}

#[test]
fn assign() -> TestResult {
    match_ssa!(r"
        let x = 1
        x = 2
        x = 3
        ",
        Statement { target: one, op: SSAOp::Int(one_value) },
        Statement { target: two, op: SSAOp::Int(two_value) },
        Statement { target: three, op: SSAOp::Int(three_value) },
        =>
        assert_eq!(*one_value, BigInt::from(1u8));
        assert_eq!(*two_value, BigInt::from(2u8));
        assert_eq!(*three_value, BigInt::from(3u8));
        assert_ne!(one, two);
        assert_ne!(two, three);
        assert_eq!(one.id.name, two.id.name);
        assert_eq!(two.id.name, three.id.name);
        assert_ne!(one.id.version, two.id.version);
        assert_ne!(two.id.version, three.id.version);
    )
}

macro_rules! test_int_op {
    ($op_char: expr, $op_variant: path) => (match_ssa!(&format!("1{}2", $op_char),
        Statement { target: one, op: SSAOp::Int(one_value) },
        Statement { target: two, op: SSAOp::Int(two_value) },
        Statement { op: $op_variant(left, right), .. }
        =>
        assert_eq!(*one_value, BigInt::from(1u8));
        assert_eq!(*two_value, BigInt::from(2u8));
        assert_eq!(left, &one.id);
        assert_eq!(right, &two.id);
        assert_ne!(left, right);
    ))
}

#[test]
fn add_int() -> TestResult {
    test_int_op!('+', SSAOp::AddInt)
}

#[test]
fn sub_int() -> TestResult {
    test_int_op!('-', SSAOp::SubInt)
}

#[test]
fn mul_int() -> TestResult {
    test_int_op!('*', SSAOp::MulInt)
}

#[test]
fn div_int() -> TestResult {
    test_int_op!('/', SSAOp::DivInt)
}

#[test]
fn all_int() -> TestResult {
    match_ssa!("(1 + 2) - 3 * (4 / 5)",
        Statement { target: one, op: SSAOp::Int(one_value) },
        Statement { target: two, op: SSAOp::Int(two_value) },
        Statement { target: add, op: SSAOp::AddInt(add_left, add_right) },
        Statement { target: three, op: SSAOp::Int(three_value) },
        Statement { target: four, op: SSAOp::Int(four_value) },
        Statement { target: five, op: SSAOp::Int(five_value) },
        Statement { target: div, op: SSAOp::DivInt(div_left, div_right) },
        Statement { target: mul, op: SSAOp::MulInt(mul_left, mul_right) },
        Statement { op: SSAOp::SubInt(sub_left, sub_right), .. },
        =>
        assert_eq!(*one_value, BigInt::from(1u8));
        assert_eq!(*two_value, BigInt::from(2u8));
        assert_eq!(*three_value, BigInt::from(3u8));
        assert_eq!(*four_value, BigInt::from(4u8));
        assert_eq!(*five_value, BigInt::from(5u8));
        assert_eq!(add_left, &one.id);
        assert_eq!(add_right, &two.id);
        assert_eq!(div_left, &four.id);
        assert_eq!(div_right, &five.id);
        assert_eq!(mul_left, &three.id);
        assert_eq!(mul_right, &div.id);
        assert_eq!(sub_left, &add.id);
        assert_eq!(sub_right, &mul.id);
    )
}

#[test]
fn conditional_with_both_branches() -> TestResult {
    match_ssa!("if 0 {1} else {2}",
        Statement { target: condition, op: SSAOp::Int(condition_value) },
        Statement { op: SSAOp::Cbz(cbz_condition, cbz_negative_label), .. },
        Statement { target: positive, op: SSAOp::Int(positive_value) },
        Statement { op: SSAOp::Br(br_end_label), .. },
        Statement { target: negative_label, op: SSAOp::Label },
        Statement { target: negative, op: SSAOp::Int(negative_value) },
        Statement { target: end_label, op: SSAOp::Label },
        Statement { op: SSAOp::Phi(phi_positive, phi_negative), .. },
        =>
        assert_eq!(*condition_value, BigInt::from(0u8));
        assert_eq!(*positive_value, BigInt::from(1u8));
        assert_eq!(*negative_value, BigInt::from(2u8));
        assert_eq!(cbz_condition, &condition.id);
        assert_eq!(cbz_negative_label, &negative_label.id);
        assert_eq!(br_end_label, &end_label.id);
        assert_eq!(phi_positive, &positive.id);
        assert_eq!(phi_negative, &negative.id);
    )
}

#[test]
fn conditional_with_only_positive_branch() -> TestResult {
    match_ssa!("if 1 {2}",
        Statement { target: condition, op: SSAOp::Int(condition_value) },
        Statement { op: SSAOp::Cbz(cbz_condition, cbz_negative_label), .. },
        Statement { op: SSAOp::Int(positive_value), .. },
        Statement { target: positive, op: SSAOp::Unit },
        Statement { op: SSAOp::Br(br_end_label), .. },
        Statement { target: negative_label, op: SSAOp::Label },
        Statement { target: negative, op: SSAOp::Unit },
        Statement { target: end_label, op: SSAOp::Label },
        Statement { op: SSAOp::Phi(phi_positive, phi_negative), .. },
        =>
        assert_eq!(*condition_value, BigInt::from(1u8));
        assert_eq!(*positive_value, BigInt::from(2u8));
        assert_eq!(cbz_condition, &condition.id);
        assert_eq!(cbz_negative_label, &negative_label.id);
        assert_eq!(br_end_label, &end_label.id);
        assert_eq!(phi_positive, &positive.id);
        assert_eq!(phi_negative, &negative.id);
    )
}

#[test]
fn assignment_phi() -> TestResult {
    match_ssa!(r"
        let x = 0
        if (x) {
            x = 1
            2
        } else {
            x = 3
            4
        }
        ",
        Statement { target: x_definition, op: SSAOp::Int(x_initial_value) },
        Statement { .. }, // cbz x @negative
        Statement { target: x_positive, op: SSAOp::Int(one) }, // x = 1
        Statement { .. }, // 2
        Statement { .. }, // br @end
        Statement { .. }, // @negative
        Statement { target: x_negative, op: SSAOp::Int(three) }, // x = 3
        Statement { .. }, // 4
        Statement { .. }, // @end
        Statement { target: x_final, op: SSAOp::Phi(phi_x1, phi_x2) }, // ϕ(x = 1, x = 2)
        Statement { .. }, // ϕ(positive, negative)
        =>
        assert_eq!(*x_initial_value,  BigInt::from(0u8));
        assert_eq!(*one,  BigInt::from(1u8));
        assert_eq!(*three,  BigInt::from(3u8));
        assert_eq!(x_positive.id.name, x_definition.id.name);
        assert_eq!(x_negative.id.name, x_definition.id.name);
        assert_eq!(x_final.id.name, x_definition.id.name);
        assert_eq!(phi_x1, &x_positive.id);
        assert_eq!(phi_x2, &x_negative.id);
    )
}

#[test]
fn phi_inner_conditional_assignment() -> TestResult {
    match_ssa!("
        let x = 0
        if (1) {
            if (2) x = 3
        }
        ",
        Statement { target: x_definition, .. }, // let x = 0
        Statement {..}, // 1
        Statement {..}, // jump to negative branch
        Statement {..}, // 2
        Statement {..}, // jump to negative branch
        Statement { target: x_assignment, .. }, // x = 3
        Statement {..}, // x = 3
        Statement {..}, // jump to the end of conditional
        Statement {..}, // negative branch
        Statement {..}, // negative branch value
        Statement {..}, // end of conditional
        Statement { target: inner_phi, op: SSAOp::Phi(inner_phi_left, inner_phi_right) },
        Statement {..}, // if (2) x = 3
        Statement {..}, // { if (2) x = 3 }
        Statement {..}, // jump to the end of conditional
        Statement {..}, // negative branch
        Statement {..}, // negative branch value
        Statement {..}, // end of conditional
        Statement { target: outer_phi, op: SSAOp::Phi(outer_phi_left, outer_phi_right) },
        Statement {..}, // if (1) { if (2) x = 3 }
        =>
        assert_eq!(x_assignment.id.name, x_definition.id.name);
        assert_eq!(inner_phi.id.name, x_definition.id.name);
        assert_eq!(inner_phi_left.name, x_definition.id.name);
        assert_eq!(inner_phi_right.name, x_definition.id.name);
        assert_eq!(outer_phi.id.name, x_definition.id.name);
        assert_eq!(outer_phi_left.name, x_definition.id.name);
        assert_eq!(outer_phi_right.name, x_definition.id.name);
        assert_ne!(x_assignment.id, x_definition.id);
        assert_ne!(inner_phi.id, x_assignment.id);
        assert_ne!(outer_phi.id, inner_phi.id);
        assert_eq!(inner_phi_left, &x_assignment.id);
        assert_eq!(inner_phi_right, &x_definition.id);
        assert_eq!(outer_phi_left, &inner_phi.id);
        assert_eq!(outer_phi_right, &x_definition.id);
    )
}
