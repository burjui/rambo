use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Formatter;

use num_bigint::BigInt;

use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use crate::unique_rc::UniqueRc;

crate struct Codegen {
    ssa: Vec<Statement>,
    next_id: usize,
    ids: HashMap<BindingRef, SSAId>
}

impl Codegen {
    crate fn new() -> Self {
        Self {
            ids: HashMap::new(),
            next_id: 0,
            ssa: Vec::new()
        }
    }

    crate fn build(mut self, expr: &ExprRef) -> Vec<Statement> {
        self.process_expr(expr, None);
        self.ssa
    }

    fn process_expr(&mut self, expr: &ExprRef, target: Option<SSAId>) -> usize {
        match &**expr {
            TypedExpr::Int(value, source) => self.push(target, SSAOp::Int(value.clone()), source.text().to_owned()),
            TypedExpr::Assign(binding, value, source) => {
                let new_version = SSAId::next(&self.id(binding));
                self.ids.insert(binding.clone(), new_version.clone());
                let statement = self.process_expr(value, Some(new_version));
                self.get_mut(statement).comment = source.text().to_owned();
                statement
            },
            TypedExpr::AddInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::AddInt),
            TypedExpr::SubInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::SubInt),
            TypedExpr::MulInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::MulInt),
            TypedExpr::DivInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::DivInt),
            TypedExpr::Block(Block { statements, source }) => {
                if statements.is_empty() {
                    self.push(target, SSAOp::Unit, source.text().to_owned())
                } else {
                    let (head, tail) = statements.split_at(statements.len() - 1);
                    for statement in head {
                        self.process_statement(statement, None);
                    }
                    self.process_statement(tail.first().unwrap(), target)
                }
            },
            TypedExpr::Conditional { condition, positive, negative, source } => {
                let condition = self.process_expr(condition, None);
                let condition_target = self.target(condition).clone();
                let end_label = self.new_label();
                let negative_label = self.new_label();
                self.push(None, SSAOp::Cbz(condition_target.clone(), negative_label.clone()), "jump over positive branch".to_owned());
                let positive = self.process_expr(positive, None);
                self.push(None, SSAOp::Br(end_label.clone()), "jump to the end of conditional".to_owned());
                self.push(None, SSAOp::Label(negative_label), "negative branch".to_owned());
                let negative = match negative {
                    Some(negative) => self.process_expr(negative, None),
                    None => self.push(None, SSAOp::Unit, "negative branch value".to_owned())
                };
                self.push(Some(end_label.clone()), SSAOp::Label(end_label), "end of conditional".to_owned());
                self.push(target, SSAOp::Phi(self.target(positive), self.target(negative)), source.text().to_owned())
            },
            TypedExpr::Unit(source) => self.push(target, SSAOp::Unit, source.text().to_owned()),
            _ => unimplemented!("{:?}", expr)
        }
    }

    fn push_int_op(&mut self, target: Option<SSAId>, left: &ExprRef, right: &ExprRef,
                   source: &Source, constructor: impl Fn(SSAId, SSAId) -> SSAOp) -> usize
    {
        let left = self.process_expr(left, None);
        let right = self.process_expr(right, None);
        self.push(target, constructor(self.target(left), self.target(right)), source.text().to_owned())
    }

    fn process_statement(&mut self, statement: &TypedStatement, target: Option<SSAId>) -> usize {
        match statement {
            TypedStatement::Binding(binding) => {
                let target = self.id(binding);
                let value = self.process_expr(&binding.data, Some(target));
                self.get_mut(value).comment = binding.source.text().to_owned();
                value
            },
            TypedStatement::Expr(expr) => self.process_expr(expr, target)
        }
    }

    fn get(&self, index: usize) -> &Statement {
        &self.ssa[index]
    }

    fn get_mut(&mut self, index: usize) -> &mut Statement {
        &mut self.ssa[index]
    }

    fn target(&self, index: usize) -> SSAId {
        self.get(index).target.clone()
    }

    fn push(&mut self, target: Option<SSAId>, op: SSAOp, comment: String) -> usize {
        let target = if let Some(id) = target {
            id
        } else {
            self.new_id()
        };
        let statement = Statement::new(target, op, comment);
        self.ssa.push(statement.clone());
        self.ssa.len() - 1
    }

    fn new_id(&mut self) -> SSAId {
        self.new_id_from("@tmp")
    }

    fn new_label(&mut self) -> SSAId {
        self.new_id_from("@label")
    }

    fn new_id_from(&mut self, s: &str) -> SSAId {
        let name = UniqueRc::from(format!("{}{}", s, self.next_id));
        self.next_id += 1;
        SSAId::new(name)
    }

    fn id(&mut self, binding: &BindingRef) -> SSAId {
        if let Some(id) = self.ids.get(binding) {
            id.clone()
        } else {
            let id = self.new_id_from(&binding.name);
            self.ids.insert(binding.clone(), id.clone());
            id
        }
    }
}

#[derive(Clone)]
crate struct Statement {
    crate target: SSAId,
    crate op: SSAOp,
    crate comment: String,
}

impl Statement {
    fn new(target: SSAId, op: SSAOp, comment: String) -> Self {
        Self { target, op, comment }
    }
}

impl Debug for Statement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        if let SSAOp::Label(label) = &self.op {
            write!(formatter, "{:?}:", label)?;
        } else {
            write!(formatter, "    {:?} = {:?}", self.target, self.op)?;
        }
        let comment = self.comment.replace("\n", "; ");
        write!(formatter, "    // {}", comment)
    }
}

#[derive(Clone, PartialEq)]
crate enum SSAOp {
    Phi(SSAId, SSAId),
    Unit,
    Int(BigInt),
    AddInt(SSAId, SSAId),
    SubInt(SSAId, SSAId),
    MulInt(SSAId, SSAId),
    DivInt(SSAId, SSAId),
    Label(SSAId),
    Br(SSAId),
    Cbz(SSAId, SSAId)
}

impl Debug for SSAOp {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SSAOp::Phi(left, right) => write!(formatter, "ϕ({:?}, {:?})", left, right),
            SSAOp::Unit => formatter.write_str("()"),
            SSAOp::Int(value) => write!(formatter, "{}", value),
            SSAOp::AddInt(left, right) => write!(formatter, "{:?} + {:?}", left, right),
            SSAOp::SubInt(left, right) => write!(formatter, "{:?} - {:?}", left, right),
            SSAOp::MulInt(left, right) => write!(formatter, "{:?} * {:?}", left, right),
            SSAOp::DivInt(left, right) => write!(formatter, "{:?} / {:?}", left, right),
            SSAOp::Label(id) => write!(formatter, "{}", &id.name),
            SSAOp::Br(label) => write!(formatter, "br {:?}", label),
            SSAOp::Cbz(condition, label) => write!(formatter, "cbz {:?} {:?}", condition, label),
        }
    }
}

#[derive(Clone, PartialEq)]
crate struct SSAId {
    crate name: UniqueRc<String>,
    crate version: usize
}

impl SSAId {
    fn new(name: UniqueRc<String>) -> SSAId {
        SSAId {
            name: name.clone(),
            version: 0
        }
    }

    fn next(id: &SSAId) -> SSAId {
        SSAId {
            name: id.name.clone(),
            version: id.version + 1
        }
    }
}

impl Debug for SSAId {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}'{}", self.name, self.version)
    }
}

#[cfg(test)]
macro_rules! match_ssa {
    ($code: expr $(, $patterns: pat)+ $(,)? => $($handlers: stmt;)*) => ({
        let ssa = Codegen::new().build(&typecheck!($code)?);
        match &ssa as &[Statement] {
            [$($patterns, )*] => Ok({
                $($handlers;)*
            }),
            _ => {
                use itertools::Itertools;
                panic!("\n\nvalue doesn't match the pattern:\n  {:#?}\n\n", ssa.iter().format("\n  "))
            }
        }
    })
}

type TestResult = Result<(), Box<dyn Error>>;

#[test]
fn assign() -> TestResult {
    match_ssa!(r"
        let x = 1
        x = 2
        x = 3
        ",
        Statement { target: one, op: SSAOp::Int(one_value), .. },
        Statement { target: two, op: SSAOp::Int(two_value), .. },
        Statement { target: three, op: SSAOp::Int(three_value), .. },
        =>
        assert_eq!(*one_value, BigInt::from(1u8));
        assert_eq!(*two_value, BigInt::from(2u8));
        assert_eq!(*three_value, BigInt::from(3u8));
        assert_ne!(one, two);
        assert_ne!(two, three);
        assert_eq!(one.name, two.name);
        assert_eq!(two.name, three.name);
        assert_ne!(one.version, two.version);
        assert_ne!(two.version, three.version);
    )
}

#[cfg(test)]
macro_rules! test_int_op {
    ($op_char: expr, $op_variant: path) => (match_ssa!(&format!("1{}2", $op_char),
        Statement { target: one, op: SSAOp::Int(one_value), .. },
        Statement { target: two, op: SSAOp::Int(two_value), .. },
        Statement { op: $op_variant(left, right), .. }
        =>
        assert_eq!(*one_value, BigInt::from(1u8));
        assert_eq!(*two_value, BigInt::from(2u8));
        assert_eq!(left, one);
        assert_eq!(right, two);
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
        Statement { target: one, op: SSAOp::Int(one_value), .. },
        Statement { target: two, op: SSAOp::Int(two_value), .. },
        Statement { target: add, op: SSAOp::AddInt(add_left, add_right), .. },
        Statement { target: three, op: SSAOp::Int(three_value), .. },
        Statement { target: four, op: SSAOp::Int(four_value), .. },
        Statement { target: five, op: SSAOp::Int(five_value), .. },
        Statement { target: div, op: SSAOp::DivInt(div_left, div_right), .. },
        Statement { target: mul, op: SSAOp::MulInt(mul_left, mul_right), .. },
        Statement { op: SSAOp::SubInt(sub_left, sub_right), .. },
        =>
        assert_eq!(*one_value, BigInt::from(1u8));
        assert_eq!(*two_value, BigInt::from(2u8));
        assert_eq!(*three_value, BigInt::from(3u8));
        assert_eq!(*four_value, BigInt::from(4u8));
        assert_eq!(*five_value, BigInt::from(5u8));
        assert_eq!(add_left, one);
        assert_eq!(add_right, two);
        assert_eq!(div_left, four);
        assert_eq!(div_right, five);
        assert_eq!(mul_left, three);
        assert_eq!(mul_right, div);
        assert_eq!(sub_left, add);
        assert_eq!(sub_right, mul);
    )
}

#[test]
fn conditional_with_both_branches() -> TestResult {
    match_ssa!("if 0 {1} else {2}",
        Statement { target: condition, op: SSAOp::Int(condition_value), .. },
        Statement { op: SSAOp::Cbz(cbz_condition, cbz_negative_label), .. },
        Statement { target: positive, op: SSAOp::Int(positive_value), .. },
        Statement { op: SSAOp::Br(br_end_label), .. },
        Statement { op: SSAOp::Label(negative_label), .. },
        Statement { target: negative, op: SSAOp::Int(negative_value), .. },
        Statement { op: SSAOp::Label(end_label), .. },
        Statement { op: SSAOp::Phi(phi_positive, phi_negative), .. },
        =>
        assert_eq!(*condition_value, BigInt::from(0u8));
        assert_eq!(*positive_value, BigInt::from(1u8));
        assert_eq!(*negative_value, BigInt::from(2u8));
        assert_eq!(cbz_condition, condition);
        assert_eq!(cbz_negative_label, negative_label);
        assert_eq!(br_end_label, end_label);
        assert_eq!(phi_positive, positive);
        assert_eq!(phi_negative, negative);
    )
}

#[test]
fn conditional_with_only_positive_branch() -> TestResult {
    match_ssa!("if 1 {2}",
        Statement { target: condition, op: SSAOp::Int(condition_value), .. },
        Statement { op: SSAOp::Cbz(cbz_condition, cbz_negative_label), .. },
        Statement { op: SSAOp::Int(positive_value), .. },
        Statement { target: positive, op: SSAOp::Unit, .. },
        Statement { op: SSAOp::Br(br_end_label), .. },
        Statement { op: SSAOp::Label(negative_label), .. },
        Statement { target: negative, op: SSAOp::Unit, .. },
        Statement { op: SSAOp::Label(end_label), .. },
        Statement { op: SSAOp::Phi(phi_positive, phi_negative), .. },
        =>
        assert_eq!(*condition_value, BigInt::from(1u8));
        assert_eq!(*positive_value, BigInt::from(2u8));
        assert_eq!(cbz_condition, condition);
        assert_eq!(cbz_negative_label, negative_label);
        assert_eq!(br_end_label, end_label);
        assert_eq!(phi_positive, positive);
        assert_eq!(phi_negative, negative);
    )
}
