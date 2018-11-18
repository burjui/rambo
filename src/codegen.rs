use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;

use lazy_static::lazy_static;
use num_bigint::BigInt;
use regex::Regex;

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
    ids: HashMap<BindingRef, SSAId>,
    modified: HashSet<Modified>,
}

impl Codegen {
    crate fn new() -> Self {
        Self {
            ssa: Vec::new(),
            next_id: 0,
            ids: HashMap::new(),
            modified: HashSet::new(),
        }
    }

    crate fn build(mut self, expr: &ExprRef) -> Vec<Statement> {
        self.process_expr(expr, None);
        self.ssa
    }

    fn process_expr(&mut self, expr: &ExprRef, target: Option<Target>) -> SSAId {
        match &**expr {
            TypedExpr::Int(value, source) => self.push(target, SSAOp::Int(value.clone()), source.text()),
            TypedExpr::Assign(binding, value, source) => {
                let new_version = self.next_version(binding, source.text());
                self.modified.insert(Modified {
                    binding: binding.clone(),
                    id: new_version.id.clone()
                });
                self.process_expr(value, Some(new_version))
            },
            TypedExpr::AddInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::AddInt),
            TypedExpr::SubInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::SubInt),
            TypedExpr::MulInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::MulInt),
            TypedExpr::DivInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::DivInt),
            TypedExpr::Block(Block { statements, source }) => {
                if statements.is_empty() {
                    self.push(target, SSAOp::Unit, source.text())
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
                let negative_label = self.new_label("negative branch");
                self.push(None, SSAOp::Cbz(condition, negative_label.id.clone()), "jump to negative branch");
                let positive = self.process_expr(positive, None);
                let modified_in_positive = self.take_modified();
                let end_label = self.new_label("end of conditional");
                self.push(None, SSAOp::Br(end_label.id.clone()), "jump to the end of conditional");
                self.push(Some(negative_label.clone()), SSAOp::Label, "");
                let negative = match negative {
                    Some(negative) => self.process_expr(negative, None),
                    None => self.push(None, SSAOp::Unit, "negative branch value")
                };
                let modified_in_negative = self.take_modified();
                let needs_phi = modified_in_positive.intersection(&modified_in_negative)
                    .map(|modified| (&modified.binding, &modified_in_positive.get(modified).unwrap().id, &modified_in_negative.get(modified).unwrap().id));
                self.push(Some(end_label.clone()), SSAOp::Label, "");
                for (binding, id1, id2) in needs_phi {
                    let new_version = self.next_version(binding, "modified in both branches");
                    self.push(Some(new_version), SSAOp::Phi(id1.clone(), id2.clone()), "");
                }
                self.push(target, SSAOp::Phi(positive, negative), source.text())
            },
            TypedExpr::Unit(source) => self.push(target, SSAOp::Unit, source.text()),
            TypedExpr::Reference(binding, _) => self.id(binding),

            TypedExpr::ArgumentPlaceholder(_, _) |
            TypedExpr::String(_, _) |
            TypedExpr::Lambda(_, _) |
            TypedExpr::Application { .. } |
            TypedExpr::AddStr(_, _, _) => unimplemented!("{:?}", expr)
        }
    }

    fn process_statement(&mut self, statement: &TypedStatement, target: Option<Target>) -> SSAId {
        match statement {
            TypedStatement::Binding(binding) => {
                let target = Target::new(self.id(binding), binding.source.text());
                self.process_expr(&binding.data, Some(target))
            },
            TypedStatement::Expr(expr) => self.process_expr(expr, target)
        }
    }

    fn take_modified(&mut self) -> HashSet<Modified> {
        self.modified.drain().collect::<HashSet<_>>()
    }

    fn push(&mut self, target: Option<Target>, op: SSAOp, comment: &str) -> SSAId {
        let target = target.unwrap_or_else(|| self.new_target(comment));
        self.ssa.push(Statement::new(target.clone(), op));
        target.id
    }

    fn push_int_op(&mut self, target: Option<Target>, left: &ExprRef, right: &ExprRef,
                   source: &Source, constructor: impl Fn(SSAId, SSAId) -> SSAOp) -> SSAId
    {
        let left = self.process_expr(left, None);
        let right = self.process_expr(right, None);
        self.push(target, constructor(left, right), source.text())
    }

    fn new_label(&mut self, comment: &str) -> Target {
        Target::new(self.new_id_from("@label"), comment)
    }

    fn new_target(&mut self, comment: &str) -> Target {
        Target::new(self.new_id(), comment)
    }

    fn new_id(&mut self) -> SSAId {
        self.new_id_from("@tmp")
    }

    fn new_id_from(&mut self, prefix: &str) -> SSAId {
        let name = UniqueRc::from(format!("{}{}", prefix, self.next_id));
        self.new_ssa_id(SSAIdName::String(name))
    }

    fn id(&mut self, binding: &BindingRef) -> SSAId {
        if let Some(id) = self.ids.get(binding) {
            id.clone()
        } else {
            let id = self.new_ssa_id(SSAIdName::Binding(binding.clone()));
            self.ids.insert(binding.clone(), id.clone());
            id
        }
    }

    fn new_ssa_id(&mut self, name: SSAIdName) -> SSAId {
        SSAId {
            name,
            version: 0,
            unique_id: self.unique_id()
        }
    }

    fn next_version(&mut self, binding: &BindingRef, comment: &str) -> Target {
        let id = self.id(binding);
        let new_id = SSAId {
            name: id.name.clone(),
            version: id.version + 1,
            unique_id: self.unique_id()
        };
        self.ids.insert(binding.clone(), new_id.clone());
        Target::new(new_id, comment)
    }

    fn unique_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

#[derive(Clone)]
crate struct Target {
    id: SSAId,
    comment: String
}

impl Target {
    fn new(id: SSAId, comment: &str) -> Self {
        Self { id, comment: comment.to_owned() }
    }
}

impl PartialEq for Target {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Target {}

impl Hash for Target {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl Debug for Target {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        self.id.fmt(formatter)
    }
}

#[derive(Clone)]
crate struct Statement {
    crate target: Target,
    crate op: SSAOp
}

impl Statement {
    fn new(target: Target, op: SSAOp) -> Self {
        Self { target, op }
    }
}

impl Debug for Statement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let code = if let SSAOp::Label = &self.op {
            format!("{:?}:", &self.target.id)
        } else {
            let s = match &self.op {
                SSAOp::Br(_) | SSAOp::Cbz(_, _) => format!("{:?}", &self.op),
                _ => format!("{:?} = {:?}", &self.target.id, &self.op)
            };
            format!("    {}", s)
        };

        lazy_static! {
            static ref WHITESPACE_REGEX: Regex = Regex::new(r"[ \t]+").unwrap();
        }
        let comment = self.target.comment.replace("\n", " ");
        let comment = WHITESPACE_REGEX.replace_all(&comment, " ");
        write!(formatter, "{:40}    // {}", code, comment)
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
    Label,
    Br(SSAId),
    Cbz(SSAId, SSAId),
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
            SSAOp::Label => formatter.write_str("label"),
            SSAOp::Br(label) => write!(formatter, "br {:?}", label),
            SSAOp::Cbz(condition, label) => write!(formatter, "cbz {:?} {:?}", condition, label),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
crate enum SSAIdName {
    String(UniqueRc<String>),
    Binding(BindingRef)
}

impl Debug for SSAIdName {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SSAIdName::String(s) => formatter.write_str(s),
            SSAIdName::Binding(binding) => formatter.write_str(&binding.name)
        }
    }
}

#[derive(Clone)]
crate struct SSAId {
    crate name: SSAIdName,
    crate version: usize,
    unique_id: usize
}

impl Debug for SSAId {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            SSAIdName::String(_) => Debug::fmt(&self.name, formatter),
            SSAIdName::Binding(_) => write!(formatter, "{:?}'{}", self.name, self.version)
        }
    }
}

impl PartialEq for SSAId {
    fn eq(&self, other: &Self) -> bool {
        self.unique_id == other.unique_id
    }
}

impl Eq for SSAId {}

impl Hash for SSAId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.unique_id.hash(state)
    }
}

struct Modified {
    binding: BindingRef,
    id: SSAId
}

impl PartialEq for Modified {
    fn eq(&self, other: &Modified) -> bool {
        self.binding == other.binding
    }
}

impl Eq for Modified {}

impl Hash for Modified {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.binding.hash(state)
    }
}

impl Debug for Modified {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{} -> {:?}", &self.binding.name, &self.id)
    }
}

#[cfg(test)]
#[cfg(feature = "dump_ssa")]
fn dump(code: &str, ssa: &[Statement]) -> TestResult {
    use std::fs::File;
    use std::io::Write;
    use itertools::Itertools;
    use std::sync::Arc;
    use std::sync::Mutex;

    lazy_static! {
        static ref SSA_TXT: Arc<Mutex<File>> = Arc::new(Mutex::new(File::create("ssa.txt").unwrap()));
    }

    let mut file = SSA_TXT.lock().unwrap();
    writeln!(file, "-------------\nCODE:\n{}\n\nSSA:\n{:#?}", code, ssa.iter().format("\n"))?;
    Ok(())
}

#[cfg(test)]
macro_rules! match_ssa {
    ($code: expr $(, $patterns: pat)* $(,)? => $($handlers: stmt;)*) => ({
        let ssa = Codegen::new().build(&typecheck!($code)?);
        #[cfg(feature = "dump_ssa")]
        dump($code, &ssa)?;
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

#[cfg(test)]
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
fn empty() -> TestResult {
    match_ssa!("", Statement { op: SSAOp::Unit, .. })
}
