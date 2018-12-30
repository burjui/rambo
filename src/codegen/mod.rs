use std::mem::replace;

use hashbrown::HashMap;
use hashbrown::HashSet;

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
use crate::ssa::generator::SSAIdGenerator;
use crate::ssa::SSAId;
use crate::ssa::SSAIdName;
use crate::ssa::SSAOp;
use crate::ssa::SSAStatement;
use crate::ssa::Target;

#[cfg(test)]
mod tests;

crate fn generate_ssa(expr: &ExprRef) -> Vec<SSAStatement> {
    Codegen::new().build(expr)
}

struct Codegen {
    ssa: Vec<SSAStatement>,
    id_generator: SSAIdGenerator,
    env: Environment<BindingRef, SSAId>,
    modified: HashMap<BindingRef, SSAId>,
    lambda_cache: HashMap<LambdaRef, (SSAId, Vec<SSAStatement>)>,
    unique_tmp_id: usize,
    unique_label_id: usize,
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
    fn new() -> Self {
        Self {
            ssa: Vec::new(),
            id_generator: SSAIdGenerator::new(),
            env: Environment::new(),
            modified: HashMap::new(),
            lambda_cache: HashMap::new(),
            unique_tmp_id: 0,
            unique_label_id: 0
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
                let id = self.binding_id(binding);
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
                        target.id = self.binding_id(binding);
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
                let binding_id = self.binding_id(binding);
                let id = self.process_expr(&binding.data, Some(&mut Target::new(binding_id, binding.source.text())));
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
        let new_id = self.unique_label_id + 1;
        let id = replace(&mut self.unique_label_id, new_id);
        Target::new(self.id_generator.new_id(SSAIdName::Label(id)), comment)
    }

    fn new_target(&mut self, comment: &str) -> Target {
        Target::new(self.new_tmp(), comment)
    }

    fn new_tmp(&mut self) -> SSAId {
        let new_id = self.unique_tmp_id + 1;
        let id = replace(&mut self.unique_tmp_id, new_id);
        self.id_generator.new_id(SSAIdName::Tmp(id))
    }

    fn binding_id(&mut self, binding: &BindingRef) -> SSAId {
        if let Ok(id) = self.env.resolve(binding) {
            id.clone()
        } else {
            let id = self.id_generator.new_id(SSAIdName::Binding {
                binding: binding.clone(),
                version: 0
            });
            self.env.bind(binding.clone(), id.clone());
            id
        }
    }

    fn next_version(&mut self, binding: &BindingRef, id: &SSAId, comment: &str) -> Target {
        let new_id = self.id_generator.next_version(id);
        self.env.bind(binding.clone(), new_id.clone());
        Target::new(new_id, comment)
    }
}
