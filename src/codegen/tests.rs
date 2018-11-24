use num_bigint::BigInt;
use std::error::Error;

use crate::codegen::Codegen;
use crate::codegen::SSAOp;
use crate::codegen::SSAStatement;

#[cfg(feature = "dump_ssa_in_tests")]
fn dump(code: &str, ssa: &[SSAStatement]) -> TestResult {
    use std::fs::File;
    use std::io::Write;
    use itertools::Itertools;
    use std::sync::Mutex;
    use once_cell::sync::Lazy;
    use once_cell::sync_lazy;

    static SSA_TXT: Lazy<Mutex<File>> = sync_lazy!(Mutex::new(File::create("ssa.txt").unwrap()));
    let mut file = SSA_TXT.lock().unwrap();
    writeln!(file, "-------------\nCODE:\n{}\n\nSSA:\n{:#?}", code, ssa.iter().format("\n"))?;
    Ok(())
}

macro_rules! match_ssa {
    ($code: expr $(, $patterns: pat)* $(,)? => $($handlers: stmt;)*) => ({
        let ssa = Codegen::new().build(&typecheck!($code)?);
        #[cfg(feature = "dump_ssa_in_tests")] dump($code, &ssa)?;
        match &ssa as &[SSAStatement] {
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
    match_ssa!("", SSAStatement { op: SSAOp::Unit, .. })
}

#[test]
fn assign() -> TestResult {
    match_ssa!(r"
        let x = 1
        x = 2
        x = 3
        ",
        SSAStatement { target: one, op: SSAOp::Int(one_value) },
        SSAStatement { target: two, op: SSAOp::Int(two_value) },
        SSAStatement { target: three, op: SSAOp::Int(three_value) },
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
        SSAStatement { target: one, op: SSAOp::Int(one_value) },
        SSAStatement { target: two, op: SSAOp::Int(two_value) },
        SSAStatement { op: $op_variant(left, right), .. }
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
        SSAStatement { target: one, op: SSAOp::Int(one_value) },
        SSAStatement { target: two, op: SSAOp::Int(two_value) },
        SSAStatement { target: add, op: SSAOp::AddInt(add_left, add_right) },
        SSAStatement { target: three, op: SSAOp::Int(three_value) },
        SSAStatement { target: four, op: SSAOp::Int(four_value) },
        SSAStatement { target: five, op: SSAOp::Int(five_value) },
        SSAStatement { target: div, op: SSAOp::DivInt(div_left, div_right) },
        SSAStatement { target: mul, op: SSAOp::MulInt(mul_left, mul_right) },
        SSAStatement { op: SSAOp::SubInt(sub_left, sub_right), .. },
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
        SSAStatement { target: condition, op: SSAOp::Int(condition_value) },
        SSAStatement { op: SSAOp::Cbz(cbz_condition, cbz_negative_label), .. },
        SSAStatement { target: positive, op: SSAOp::Int(positive_value) },
        SSAStatement { op: SSAOp::Br(br_end_label), .. },
        SSAStatement { target: negative_label, op: SSAOp::Label },
        SSAStatement { target: negative, op: SSAOp::Int(negative_value) },
        SSAStatement { target: end_label, op: SSAOp::Label },
        SSAStatement { op: SSAOp::Phi(phi_positive, phi_negative), .. },
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
        SSAStatement { target: condition, op: SSAOp::Int(condition_value) },
        SSAStatement { op: SSAOp::Cbz(cbz_condition, cbz_negative_label), .. },
        SSAStatement { op: SSAOp::Int(positive_value), .. },
        SSAStatement { target: positive, op: SSAOp::Unit },
        SSAStatement { op: SSAOp::Br(br_end_label), .. },
        SSAStatement { target: negative_label, op: SSAOp::Label },
        SSAStatement { target: negative, op: SSAOp::Unit },
        SSAStatement { target: end_label, op: SSAOp::Label },
        SSAStatement { op: SSAOp::Phi(phi_positive, phi_negative), .. },
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
        SSAStatement { target: x_definition, op: SSAOp::Int(x_initial_value) },
        SSAStatement { .. }, // cbz x @negative
        SSAStatement { target: x_positive, op: SSAOp::Int(one) }, // x = 1
        SSAStatement { .. }, // 2
        SSAStatement { .. }, // br @end
        SSAStatement { .. }, // @negative
        SSAStatement { target: x_negative, op: SSAOp::Int(three) }, // x = 3
        SSAStatement { .. }, // 4
        SSAStatement { .. }, // @end
        SSAStatement { target: x_final, op: SSAOp::Phi(phi_x1, phi_x2) }, // ϕ(x = 1, x = 2)
        SSAStatement { .. }, // ϕ(positive, negative)
        =>
        assert_eq!(*x_initial_value, BigInt::from(0u8));
        assert_eq!(*one, BigInt::from(1u8));
        assert_eq!(*three, BigInt::from(3u8));
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
        SSAStatement { target: x_definition, .. }, // let x = 0
        SSAStatement {..}, // 1
        SSAStatement {..}, // jump to negative branch
        SSAStatement {..}, // 2
        SSAStatement {..}, // jump to negative branch
        SSAStatement { target: x_assignment, .. }, // x = 3
        SSAStatement {..}, // x = 3
        SSAStatement {..}, // jump to the end of conditional
        SSAStatement {..}, // negative branch
        SSAStatement {..}, // negative branch value
        SSAStatement {..}, // end of conditional
        SSAStatement { target: inner_phi, op: SSAOp::Phi(inner_phi_left, inner_phi_right) },
        SSAStatement {..}, // if (2) x = 3
        SSAStatement {..}, // { if (2) x = 3 }
        SSAStatement {..}, // jump to the end of conditional
        SSAStatement {..}, // negative branch
        SSAStatement {..}, // negative branch value
        SSAStatement {..}, // end of conditional
        SSAStatement { target: outer_phi, op: SSAOp::Phi(outer_phi_left, outer_phi_right) },
        SSAStatement {..}, // if (1) { if (2) x = 3 }
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

#[test]
fn lambda() -> TestResult {
    match_ssa!(r"
        let f = \ (x:num, y:num) -> x + y
        f 1 2
        ",
        SSAStatement { target: arg_x, op: SSAOp::Int(one), .. },
        SSAStatement { target: arg_y, op: SSAOp::Int(two), .. },
        SSAStatement { target: call_result, op: SSAOp::Call(call_fn, call_arguments) },
        SSAStatement { op: SSAOp::End(end_value), .. },
        SSAStatement { target: f, op: SSAOp::Label, .. },
        SSAStatement { target: x, op: SSAOp::Arg(x_index) },
        SSAStatement { target: y, op: SSAOp::Arg(y_index) },
        SSAStatement { target: return_value, op: SSAOp::AddInt(left, right) },
        SSAStatement { op: SSAOp::Return(return_argument), .. },
        =>
        assert_eq!(*x_index, 0);
        assert_eq!(*y_index, 1);
        assert_eq!(left, &x.id);
        assert_eq!(right, &y.id);
        assert_eq!(return_argument, &return_value.id);
        assert_eq!(*one, BigInt::from(1u8));
        assert_eq!(*two, BigInt::from(2u8));
        assert_eq!(call_fn, &f.id);
        assert_eq!(call_arguments, &[arg_x.id.clone(), arg_y.id.clone()]);
        assert_eq!(end_value, &call_result.id);
    )
}

#[test]
fn add_str() -> TestResult {
    match_ssa!("\"a\" + \"b\"",
        SSAStatement { target: a, op: SSAOp::Str(a_value) },
        SSAStatement { target: b, op: SSAOp::Str(b_value) },
        SSAStatement { target: a_length, op: SSAOp::Length(length_a_arg) },
        SSAStatement { target: b_length, op: SSAOp::Length(length_b_arg) },
        SSAStatement { target: total_length, op: SSAOp::AddInt(total_length_left, total_length_right) },
        SSAStatement { target: result, op: SSAOp::Alloc(alloc_size) },
        SSAStatement { op: SSAOp::Copy(copy_left_src, copy_left_dst, copy_left_size), .. },
        SSAStatement { target: right_dst, op: SSAOp::AddInt(right_dst_base, right_dst_offset) },
        SSAStatement { op: SSAOp::Copy(copy_right_src, copy_right_dst, copy_right_size), .. },
        =>
        assert_eq!(&**a_value, "a");
        assert_eq!(&**b_value, "b");
        assert_eq!(length_a_arg, &a.id);
        assert_eq!(length_b_arg, &b.id);
        assert_eq!(total_length_left, &a_length.id);
        assert_eq!(total_length_right, &b_length.id);
        assert_eq!(alloc_size, &total_length.id);
        assert_eq!(copy_left_src, &a.id);
        assert_eq!(copy_left_dst, &result.id);
        assert_eq!(copy_left_size, &a_length.id);
        assert_eq!(right_dst_base, &result.id);
        assert_eq!(right_dst_offset, &a_length.id);
        assert_eq!(copy_right_src, &b.id);
        assert_eq!(copy_right_dst, &right_dst.id);
        assert_eq!(copy_right_size, &b_length.id);
    )
}
