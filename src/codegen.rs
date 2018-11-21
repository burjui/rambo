use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem::replace;

use lazy_static::lazy_static;
use num_bigint::BigInt;
use regex::Regex;

use crate::env::Environment;
use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use crate::unique_rc::UniqueRc;
use crate::codegen::ssaid_factory::SSAIdFactory;

#[cfg(test)] mod tests;

crate struct Codegen {
    ssa: Vec<Statement>,
    id_factory: SSAIdFactory,
    env: Environment<BindingRef, SSAId>,
    modified: HashMap<BindingRef, SSAId>,
}

impl Codegen {
    crate fn new() -> Self {
        Self {
            ssa: Vec::new(),
            id_factory: SSAIdFactory::new(),
            env: Environment::new(),
            modified: HashMap::new(),
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
                let id = self.id(binding);
                let new_version = self.next_version(binding, &id, source.text());
                self.modified.insert(binding.clone(), new_version.id.clone());
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

                self.env.push();
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
                struct Modified(BindingRef, SSAId);
                let modified = modified_in_positive.keys()
                    .chain(modified_in_negative.keys())
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .map(|binding| Modified(binding.clone(), self.env.resolve(binding).unwrap().clone()))
                    .collect::<Vec<_>>();
                self.env.pop();

                let needs_phi = modified.into_iter()
                    .map(|modified| {
                        let phi_arg_left = modified_in_positive.get(&modified.0)
                            .unwrap_or_else(|| self.env.resolve(&modified.0).unwrap())
                            .clone();
                        let phi_arg_right = modified_in_negative.get(&modified.0)
                            .unwrap_or_else(|| self.env.resolve(&modified.0).unwrap())
                            .clone();
                        (modified, phi_arg_left, phi_arg_right)
                    })
                    .collect::<Vec<_>>();
                self.push(Some(end_label.clone()), SSAOp::Label, "");
                for (Modified(binding, last_id), id1, id2) in needs_phi {
                    let phi_target = self.next_version(&binding, &last_id, "modified in a branch");
                    let new_version = phi_target.id.clone();
                    self.push(Some(phi_target), SSAOp::Phi(id1.clone(), id2.clone()), "");
                    self.modified.insert(binding.clone(), new_version.clone());
                    self.env.bind(binding.clone(), new_version);
                }
                self.push(target, SSAOp::Phi(positive, negative), source.text())
            },
            TypedExpr::Unit(source) => self.push(target, SSAOp::Unit, source.text()),
            TypedExpr::Reference(binding, _) => self.id(binding),

            TypedExpr::String(_, _) => {
                // FIXME stub
                self.push(None, SSAOp::Unit, "STUB")
            },

            TypedExpr::ArgumentPlaceholder(_, _) |
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

    fn take_modified(&mut self) -> HashMap<BindingRef, SSAId> {
        replace(&mut self.modified, HashMap::new())
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
        Target::new(self.id_factory.new_id_prefix("@label"), comment)
    }

    fn new_target(&mut self, comment: &str) -> Target {
        Target::new(self.new_id(), comment)
    }

    fn new_id(&mut self) -> SSAId {
        self.id_factory.new_id_prefix("@tmp")
    }

    fn id(&mut self, binding: &BindingRef) -> SSAId {
        if let Some(id) = self.env.resolve(binding).ok() {
            id.clone()
        } else {
            let id = self.id_factory.new_id(SSAIdName::Binding(binding.clone()));
            self.env.bind(binding.clone(), id.clone());
            id
        }
    }

    fn next_version(&mut self, binding: &BindingRef, id: &SSAId, comment: &str) -> Target {
        let new_id = self.id_factory.next_version(id);
        self.env.bind(binding.clone(), new_id.clone());
        Target::new(new_id, comment)
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

mod ssaid_factory {
    use crate::codegen::SSAId;
    use crate::codegen::SSAIdName;
    use crate::unique_rc::UniqueRc;

    crate struct SSAIdFactory {
        next_id: usize,
    }

    impl SSAIdFactory {
        crate fn new() -> Self {
            SSAIdFactory { next_id: 0 }
        }

        crate fn new_id_prefix(&mut self, prefix: &str) -> SSAId {
            let name = UniqueRc::from(format!("{}{}", prefix, self.next_id));
            self.new_id(SSAIdName::String(name))
        }

        crate fn new_id(&mut self, name: SSAIdName) -> SSAId {
            SSAId {
                name,
                version: 0,
                unique_id: self.unique_id()
            }
        }

        crate fn next_version(&mut self, id: &SSAId) -> SSAId {
            SSAId {
                name: id.name.clone(),
                version: id.version + 1,
                unique_id: self.unique_id()
            }
        }

        fn unique_id(&mut self) -> usize {
            let unique_id = self.next_id;
            self.next_id += 1;
            unique_id
        }
    }
}
