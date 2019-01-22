use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;

use crate::semantics::BindingRef;
use crate::utils::WHITESPACE_REGEX;

pub(crate) mod generator;

#[derive(Clone)]
pub(crate) struct Target {
    pub(crate) id: SSAId,
    pub(crate) comment: String
}

impl Target {
    pub(crate) fn new(id: SSAId, comment: &str) -> Self {
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
pub(crate) struct SSAStatement {
    pub(crate) target: Target,
    pub(crate) op: SSAOp
}

impl SSAStatement {
    pub(crate) fn new(target: Target, op: SSAOp) -> Self {
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
pub(crate) enum SSAOp {
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

#[derive(Clone, Eq)]
pub(crate) enum SSAIdName {
    Tmp(usize),
    Label(usize),
    Binding {
        binding: BindingRef,
        version: usize
    }
}

impl PartialEq for SSAIdName {
    fn eq(&self, other: &SSAIdName) -> bool {
        match (self, other) {
            (SSAIdName::Tmp(this), SSAIdName::Tmp(other)) => this == other,
            (SSAIdName::Label(this), SSAIdName::Label(other)) => this == other,
            (SSAIdName::Binding { binding: this, .. }, SSAIdName::Binding { binding: other, .. }) => this == other,
            _ => false
        }
    }
}

impl Hash for SSAIdName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SSAIdName::Tmp(id) => Hash::hash(id, state),
            SSAIdName::Label(id) => Hash::hash(id, state),
            SSAIdName::Binding { binding, .. } => Hash::hash(binding, state),
        }
    }
}

impl Debug for SSAIdName {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SSAIdName::Tmp(id) => write!(formatter, "@tmp{}", id),
            SSAIdName::Label(id) => write!(formatter, "@label{}", id),
            SSAIdName::Binding { binding, version } => write!(formatter, "{}'{}", binding.name, version),
        }
    }
}

#[derive(Clone)]
pub(crate) struct SSAId {
    pub(crate) name: SSAIdName,
    unique_id: usize
}

impl Debug for SSAId {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.name, formatter)
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
