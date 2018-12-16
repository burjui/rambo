use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem::replace;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;
use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use regex::Regex;

use crate::codegen::ssaid_factory::SSAIdFactory;
use crate::env::Environment;
use crate::semantics::BindingKind;
use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::LambdaRef;
use crate::semantics::Type;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use crate::unique_rc::UniqueRc;

#[cfg(test)] mod tests;

crate struct Codegen {
    ssa: Vec<SSAStatement>,
    id_factory: SSAIdFactory,
    env: Environment<BindingRef, SSAId>,
    modified: HashMap<BindingRef, SSAId>,
    lambda_cache: HashMap<LambdaRef, (SSAId, Vec<SSAStatement>)>
}

trait OptionCloneOrElse<T> {
    fn clone_or_else(self, f: impl FnMut() -> T) -> T;
}

impl<T: Clone> OptionCloneOrElse<T> for Option<&T> {
    fn clone_or_else(self, f: impl FnMut() -> T) -> T {
        self.cloned().unwrap_or_else(f)
    }
}

impl<T: Clone> OptionCloneOrElse<T> for Option<&mut T> {
    fn clone_or_else(self, f: impl FnMut() -> T) -> T {
        self.cloned().unwrap_or_else(f)
    }
}

impl Codegen {
    crate fn new() -> Self {
        Self {
            ssa: Vec::new(),
            id_factory: SSAIdFactory::new(),
            env: Environment::new(),
            modified: HashMap::new(),
            lambda_cache: HashMap::new()
        }
    }

    crate fn build(mut self, expr: &ExprRef) -> Vec<SSAStatement> {
        let result = self.process_expr(expr, None);
        if !self.lambda_cache.is_empty() {
            let result = if let Type::Unit = expr.type_() {
                let target = self.new_target("program result");
                self.push(target, SSAOp::Unit)
            } else {
                result
            };
            let target = self.new_target("");
            self.push(target, SSAOp::End(result));
            for (_, code) in self.lambda_cache.values_mut() {
                self.ssa.append(code);
            }
        }
        self.ssa
    }

    fn process_expr(&mut self, expr: &ExprRef, target: Option<&mut Target>) -> SSAId {
        match &**expr {
            TypedExpr::Int(value, source) => {
                let target = target.clone_or_else(|| self.new_target(source.text()));
                self.push(target, SSAOp::Int(value.clone()))
            },
            TypedExpr::Assign(binding, value, source) => {
                let id = self.id(binding);
                let mut new_version = self.next_version(binding, &id, source.text());
                self.modified.insert(binding.clone(), new_version.id.clone());
                self.process_expr(value, Some(&mut new_version))
            },
            TypedExpr::AddInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::AddInt),
            TypedExpr::SubInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::SubInt),
            TypedExpr::MulInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::MulInt),
            TypedExpr::DivInt(left, right, source) => self.push_int_op(target, left, right, source, SSAOp::DivInt),
            TypedExpr::Block(Block { statements, source }) => {
                if statements.is_empty() {
                    let target = target.clone_or_else(|| self.new_target(source.text()));
                    self.push(target, SSAOp::Unit)
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
                let cbz_target = self.new_target("jump to negative branch");
                self.push(cbz_target, SSAOp::Cbz(condition, negative_label.id.clone()));

                self.env.push();
                let positive = self.process_expr(positive, None);
                let modified_in_positive = self.take_modified();
                let end_label = self.new_label("end of conditional");
                let br_target = self.new_target("jump to the end of conditional");
                self.push(br_target, SSAOp::Br(end_label.id.clone()));
                self.push(negative_label, SSAOp::Label);
                let negative = match negative {
                    Some(negative) => self.process_expr(negative, None),
                    None => {
                        let unit_target = self.new_target("negative branch value");
                        self.push(unit_target, SSAOp::Unit)
                    }
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
                self.push(end_label, SSAOp::Label);
                for (Modified(binding, last_id), id1, id2) in needs_phi {
                    let phi_target = self.next_version(&binding, &last_id, "modified in a branch");
                    let new_version = phi_target.id.clone();
                    self.push(phi_target, SSAOp::Phi(id1.clone(), id2.clone()));
                    self.modified.insert(binding.clone(), new_version.clone());
                    self.env.bind(binding.clone(), new_version);
                }
                let target = target.clone_or_else(|| self.new_target(source.text()));
                self.push(target, SSAOp::Phi(positive, negative))
            },
            TypedExpr::Unit(source) => {
                let target = target.clone_or_else(|| self.new_target(source.text()));
                self.push(target, SSAOp::Unit)
            },
            TypedExpr::Reference(binding, source) => {
                match binding.kind {
                    BindingKind::Let => {
                        let mut target = target.clone_or_else(|| self.new_target(source.text()));
                        target.id = self.id(binding);
                        target.id.clone()
                    },
                    BindingKind::Arg(index) => {
                        let target = target.clone_or_else(|| self.new_target(source.text()));
                        self.push(target, SSAOp::Arg(index))
                    }
                }
            },
            // TODO closures
            TypedExpr::Lambda(lambda, source) => {
                if let Some((id, _)) = self.lambda_cache.get(lambda) {
                    id.clone()
                } else {
                    let code = replace(&mut self.ssa, Vec::new());
                    let target = target.clone_or_else(|| self.new_target(source.text()));
                    let label = self.push(target, SSAOp::Label);
                    let return_value = self.process_expr(&lambda.body, None);
                    let return_target = self.new_target("");
                    self.push(return_target, SSAOp::Return(return_value));
                    let lambda_code = replace(&mut self.ssa, code);
                    self.lambda_cache.insert(lambda.clone(), (label.clone(), lambda_code));
                    label
                }
            },
            TypedExpr::Application { function, arguments, source, .. } => {
                let function = self.process_expr(function, None);
                let arguments = arguments.iter().map(|argument| self.process_expr(argument, None)).collect();
                let target = target.clone_or_else(|| self.new_target(source.text()));
                self.push(target, SSAOp::Call(function, arguments))
            },
            TypedExpr::String(str, source) => {
                let target = target.clone_or_else(|| self.new_target(source.text()));
                self.push(target, SSAOp::Str(str.clone()))
            },
            TypedExpr::AddStr(left, right, source) => {
                let left = self.process_expr(left, None);
                let right = self.process_expr(right, None);
                let left_length_target = self.new_target("left length");
                let left_length = self.push(left_length_target, SSAOp::Length(left.clone()));
                let right_length_target = self.new_target("right length");
                let right_length = self.push(right_length_target, SSAOp::Length(right.clone()));
                let total_length_target = self.new_target("total length");
                let total_length = self.push(total_length_target, SSAOp::AddInt(left_length.clone(), right_length.clone()));
                let allocated_memory_target = target.clone_or_else(|| self.new_target(source.text()));
                let allocated_memory = self.push(allocated_memory_target, SSAOp::Alloc(total_length));
                let copy_left_target = self.new_target("copy left");
                self.push(copy_left_target, SSAOp::Copy(left, allocated_memory.clone(), left_length.clone()));
                let right_dst_target = self.new_target("right dst");
                let right_dst = self.push(right_dst_target, SSAOp::Offset(allocated_memory.clone(), left_length));
                let copy_right_target = self.new_target("copy right");
                self.push(copy_right_target, SSAOp::Copy(right, right_dst, right_length));
                allocated_memory
            },
            TypedExpr::ArgumentPlaceholder(_, _) => unreachable!()
        }
    }

    fn process_statement(&mut self, statement: &TypedStatement, target: Option<&mut Target>) -> SSAId {
        match statement {
            TypedStatement::Binding(binding) => {
                let id = self.process_expr(&binding.data, None);
                self.env.bind(binding.clone(), id.clone());
                id
            },
            TypedStatement::Expr(expr) => self.process_expr(expr, target)
        }
    }

    fn take_modified(&mut self) -> HashMap<BindingRef, SSAId> {
        replace(&mut self.modified, HashMap::new())
    }

    fn push(&mut self, target: Target, op: SSAOp) -> SSAId {
        let id = target.id.clone();
        self.ssa.push(SSAStatement::new(target, op));
        id
    }

    fn push_int_op(&mut self, target: Option<&mut Target>, left: &ExprRef, right: &ExprRef,
                   source: &Source, constructor: impl Fn(SSAId, SSAId) -> SSAOp) -> SSAId
    {
        let left = self.process_expr(left, None);
        let right = self.process_expr(right, None);
        let target = target.clone_or_else(|| self.new_target(source.text()));
        self.push(target, constructor(left, right))
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
        if let Ok(id) = self.env.resolve(binding) {
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
    crate id: SSAId,
    crate comment: String
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
crate struct SSAStatement {
    crate target: Target,
    crate op: SSAOp
}

impl SSAStatement {
    fn new(target: Target, op: SSAOp) -> Self {
        Self { target, op }
    }
}

impl Debug for SSAStatement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let code = if let SSAOp::Label = &self.op {
            format!("{:?}:", &self.target.id)
        } else {
            let s = match &self.op {
                SSAOp::Br(_) | SSAOp::Cbz(_, _) | SSAOp::Return(_) => format!("{:?}", &self.op),
                _ => format!("{:?} = {:?}", &self.target.id, &self.op)
            };
            format!("    {}", s)
        };

        static WHITESPACE_REGEX: Lazy<Regex> = sync_lazy!(Regex::new(r"[ \t]+").unwrap());
        let comment = self.target.comment.replace("\n", " ");
        let comment = WHITESPACE_REGEX.replace_all(&comment, " ");
        if !comment.is_empty() {
            write!(formatter, "{:40}    // {}", code, comment)
        } else {
            formatter.write_str(&code)
        }
    }
}

#[derive(Clone, PartialEq)]
crate enum SSAOp {
    Unit,
    Int(BigInt),
    Str(Rc<String>),
    AddInt(SSAId, SSAId),
    SubInt(SSAId, SSAId),
    MulInt(SSAId, SSAId),
    DivInt(SSAId, SSAId),
    Offset(SSAId, SSAId),
    Label,
    Br(SSAId),
    Cbz(SSAId, SSAId),
    Arg(usize),
    Return(SSAId),
    Call(SSAId, Vec<SSAId>),
    Alloc(SSAId),
    Copy(SSAId, SSAId, SSAId),
    Length(SSAId),
    Phi(SSAId, SSAId),
    End(SSAId)
}

impl Debug for SSAOp {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SSAOp::Unit => formatter.write_str("()"),
            SSAOp::Int(value) => write!(formatter, "{}", value),
            SSAOp::Str(value) => write!(formatter, "\"{}\"", value),
            SSAOp::AddInt(left, right) => write!(formatter, "{:?} + {:?}", left, right),
            SSAOp::SubInt(left, right) => write!(formatter, "{:?} - {:?}", left, right),
            SSAOp::MulInt(left, right) => write!(formatter, "{:?} * {:?}", left, right),
            SSAOp::DivInt(left, right) => write!(formatter, "{:?} / {:?}", left, right),
            SSAOp::Offset(left, right) => write!(formatter, "&[{:?} + {:?}]", left, right),
            SSAOp::Label => formatter.write_str("label"),
            SSAOp::Br(label) => write!(formatter, "br {:?}", label),
            SSAOp::Cbz(condition, label) => write!(formatter, "cbz {:?}, {:?}", condition, label),
            SSAOp::Arg(index) => write!(formatter, "arg[{}]", index),
            SSAOp::Return(id) => write!(formatter, "ret {:?}", id),
            SSAOp::Call(id, args) => write!(formatter, "call {:?}, {:?}", id, args.iter().format(", ")),
            SSAOp::Alloc(size) => write!(formatter, "alloc {:?}", size),
            SSAOp::Copy(src, dst, size) => write!(formatter, "copy {:?}, {:?}, {:?}", src, dst, size),
            SSAOp::Length(str) => write!(formatter, "length {:?}", str),
            SSAOp::Phi(left, right) => write!(formatter, "ϕ({:?}, {:?})", left, right),
            SSAOp::End(value) => write!(formatter, "end {:?}", value)
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