use std::collections::HashMap;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;
use std::rc::Rc;

use lazy_static::lazy_static;
use num_bigint::BigInt;
use num_traits::identities::One;
use num_traits::identities::Zero;

use crate::env::Environment;
use crate::semantics::Binding;
use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::Expression;
use crate::semantics::ExprRef;
use crate::semantics::Type;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;

// TODO tests

struct BindingStats {
    usages: usize,
    assigned: bool
}

crate struct CFP {
    env: Environment<Rc<String>, BindingRef>,
    binding_stats: HashMap<BindingRef, BindingStats>,
}

type BinaryOpConstructor = fn(ExprRef, ExprRef, Source) -> TypedExpr;
type NumericFoldFn = fn(BigInt, BigInt) -> BigInt;
type BinaryOptimizeFn = fn(left: &ExprRef, right: &ExprRef, source: &Source) -> Option<ExprRef>;

trait NumericOptimizer {
    const NEW: BinaryOpConstructor;
    const FOLD: NumericFoldFn;
    const OPTIMIZE: BinaryOptimizeFn;
}

struct AddFolder;
struct SubFolder;
struct MulFolder;
struct DivFolder;

fn is_int(expr: &ExprRef, predicate: fn(&BigInt) -> bool) -> bool {
    match expr as &TypedExpr {
        TypedExpr::Int(n, _) if predicate(n) => true,
        _ => false
    }
}

fn is_zero(expr: &ExprRef) -> bool {
    is_int(expr, Zero::is_zero)
}

fn if_true_then_other(predicate: fn(&BigInt) -> bool, left: &ExprRef, right: &ExprRef) -> Option<ExprRef> {
    if is_int(left, predicate) {
        Some(right.clone())
    } else if is_int(right, predicate) {
        Some(left.clone())
    } else {
        None
    }
}

fn new_zero_expr(source: &Source) -> ExprRef {
    ExprRef::from(TypedExpr::Int(BigInt::zero(), source.clone()))
}

// TODO optimizations that require arguments without side-effects: x + x = 2 * x, x - x = 0

impl NumericOptimizer for AddFolder {
    const NEW: BinaryOpConstructor = TypedExpr::AddInt;
    const FOLD: NumericFoldFn = Add::add;
    const OPTIMIZE: BinaryOptimizeFn = |left, right, _| if_true_then_other(Zero::is_zero, left, right);
}

impl NumericOptimizer for SubFolder {
    const NEW: BinaryOpConstructor = TypedExpr::SubInt;
    const FOLD: NumericFoldFn = Sub::sub;
    const OPTIMIZE: BinaryOptimizeFn = |left, right, _| {
        if is_zero(right) {
            Some(left.clone())
        } else {
            None
        }
    };
}

impl NumericOptimizer for MulFolder {
    const NEW: BinaryOpConstructor = TypedExpr::MulInt;
    const FOLD: NumericFoldFn = Mul::mul;
    const OPTIMIZE: BinaryOptimizeFn = |left, right, source| {
        lazy_static!{ static ref NEGATIVE_ONE: BigInt = -BigInt::one(); }

        if is_zero(left) || is_zero(right) {
            Some(new_zero_expr(source))
        } else {
            if_true_then_other(One::is_one, left, right).or_else(||
                if_true_then_other(|n| n == &*NEGATIVE_ONE, left, right).and_then(|expr|
                    match &expr as &TypedExpr {
                        TypedExpr::SubInt(left, right, _) =>
                            Some(ExprRef::from(TypedExpr::SubInt(right.clone(), left.clone(), source.clone()))),
                        _ => None
                    })
            )
        }
    };
}

impl NumericOptimizer for DivFolder {
    const NEW: BinaryOpConstructor = TypedExpr::DivInt;
    const FOLD: NumericFoldFn = Div::div;
    const OPTIMIZE: BinaryOptimizeFn = |left, right, source| {
        // TODO if is_zero(right) { error_div_by_zero(); }
        if is_zero(left) {
            Some(new_zero_expr(source))
        } else if is_int(right, One::is_one) {
            Some(left.clone())
        } else {
            None
        }
    };
}

impl CFP {
    crate fn new() -> CFP {
        CFP {
            env: Environment::new(),
            binding_stats: HashMap::new()
        }
    }

    #[must_use]
    crate fn fold(&mut self, expr: &ExprRef) -> ExprRef {
        match expr as &TypedExpr {
            TypedExpr::Deref(name, _, source) => self.fold_deref(expr, name, source),
            TypedExpr::AddInt(left, right, source) => self.fold_numeric::<AddFolder>(&left, &right, source),
            TypedExpr::SubInt(left, right, source) => self.fold_numeric::<SubFolder>(&left, &right, source),
            TypedExpr::MulInt(left, right, source) => self.fold_numeric::<MulFolder>(&left, &right, source),
            TypedExpr::DivInt(left, right, source) => self.fold_numeric::<DivFolder>(&left, &right, source),
            TypedExpr::AddStr(left, right, source) => self.fold_addstr(left, right, source),
            TypedExpr::Assign(name, value, source) =>  self.fold_assign(name, value, source),
            TypedExpr::Application { type_, function, arguments, source } => self.fold_application(type_, function, arguments, source),
            TypedExpr::Lambda(_, _) => expr.clone(),
            TypedExpr::Conditional { condition, positive, negative, source } =>
                self.fold_conditional(condition, positive, negative.as_ref(), source),
            TypedExpr::Block(block) => self.fold_block(block),
            TypedExpr::ArgumentPlaceholder(_) |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => expr.clone()
        }
    }

    fn fold_deref(&mut self, expr: &ExprRef, name: &Rc<String>, source: &Source) -> ExprRef {
        let binding = self.env.resolve(name).unwrap();
        let stats = self.binding_stats(&binding);
        if stats.assigned || !binding.data.is_primitive_constant() {
            stats.usages += 1;
            expr.clone()
        } else {
            binding.data.clone_at(source.clone())
        }
    }

    fn fold_numeric<Optimizer: NumericOptimizer>(
        &mut self, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef
    {
        let (left, right) = (self.fold(left), self.fold(right));
        match (&left as &TypedExpr, &right as &TypedExpr) {
            (TypedExpr::Int(left, _), TypedExpr::Int(right, _)) => {
                let result = Optimizer::FOLD(left.clone(), right.clone());
                ExprRef::from(TypedExpr::Int(result, source.clone()))
            },
            _ => Optimizer::OPTIMIZE(&left, &right, source)
                .map(|result| self.fold(&result.clone_at(source.clone())))
                .unwrap_or_else(|| ExprRef::from(Optimizer::NEW(left.clone(), right.clone(), source.clone())))
        }
    }

    fn fold_addstr(&mut self, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef {
        let left = self.fold(left);
        let right = self.fold(right);
        match (&left as &TypedExpr, &right as &TypedExpr) {
            (TypedExpr::String(left, _), TypedExpr::String(right, _)) => {
                let mut result = String::with_capacity(left.len() + right.len());
                result.push_str(left);
                result.push_str(right);
                ExprRef::from(TypedExpr::String(Rc::new(result), source.clone()))
            },
            _ => ExprRef::from(TypedExpr::AddStr(left.clone(), right.clone(), source.clone()))
        }
    }

    fn fold_assign(&mut self, name: &Rc<String>, value: &ExprRef, source: &Source) -> ExprRef {
        let binding = self.env.resolve(name).unwrap();
        let stats = self.binding_stats(&binding);
        stats.assigned = true;
        stats.usages += 1;
        ExprRef::from(TypedExpr::Assign(name.clone(), self.fold(value), source.clone()))
    }

    fn fold_application(&mut self, type_: &Type, function: &ExprRef, arguments: &[ExprRef], source: &Source) -> ExprRef {
        let function_binding = match function as &TypedExpr {
            TypedExpr::Deref(name, _, _) => Some(self.env.resolve(name).unwrap()),
            _ => None
        };
        let function_ = function;
        let mut function = self.fold(function_);
        if function.is_primitive_constant() {
            function
        } else {
            let arguments = arguments.iter()
                .map(|argument| self.fold(argument))
                .collect::<Vec<_>>();
            if arguments.iter().all(Expression::is_constant) {
                while let TypedExpr::Deref(name, _, source) = &function.clone() as &TypedExpr {
                    let binding = self.env.resolve(name).unwrap();
                    function = binding.data.clone_at(source.clone());
                }
                if let TypedExpr::Lambda(lambda, _) = &function as &TypedExpr {
                    self.env.push();
                    for (parameter, argument) in lambda.parameters.iter().zip(arguments.iter()) {
                        let parameter_name = Rc::new(parameter.name.text().to_owned());
                        let binding = BindingRef::from(Binding::new(parameter_name, argument.clone(), parameter.name.clone()));
                        self.register_binding(binding);
                    }
                    let result = self.fold(&lambda.body);
                    self.env.pop();

                    if result.is_primitive_constant() {
                        if let Some(binding) = function_binding {
                            self.binding_stats(&binding).usages -= 1;
                        }
                        return result.clone_at(source.clone())
                    }
                }
            }
            ExprRef::from(TypedExpr::Application {
                type_: type_.clone(),
                function: function.clone(),
                arguments,
                source: source.clone(),
            })
        }
    }

    fn fold_conditional(&mut self, condition: &ExprRef, positive: &ExprRef,
                        negative: Option<&ExprRef>, source: &Source) -> ExprRef {
        let condition = self.fold(condition);
        let positive = self.fold(positive);
        let negative = negative.map(|negative| self.fold(negative));
        if let TypedExpr::Int(n, _) = &condition as &TypedExpr {
            return if n == &BigInt::zero() {
                negative.unwrap_or_else(|| ExprRef::from(TypedExpr::Unit(source.clone())))
            } else {
                positive.clone_at(source.clone())
            }
        }
        ExprRef::from(TypedExpr::Conditional {
            condition, positive, negative, source: source.clone()
        })
    }

    // TODO warn about and remove statements without side-effects, such as expressions
    fn fold_block(&mut self, block: &Block) -> ExprRef {
        self.env.push();
        let statements = block.statements.iter()
            .map(|statement| self.fold_statement(statement))
            .collect::<Vec<_>>();
        let statements = statements.iter()
            .map(|statement| match statement {
                TypedStatement::Binding(binding) =>
                    if self.binding_stats(&binding).usages > 0 {
                        statement.clone()
                    } else {
                        TypedStatement::Expr(ExprRef::from(TypedExpr::Unit(binding.source.clone())))
                    },
                TypedStatement::Expr(_) => statement.clone()
            })
            .fold(Vec::new(), |mut result, statement| {
                if let Some(TypedStatement::Expr(e1)) = result.last() {
                    if e1.is_primitive_constant() {
                        (*result.last_mut().unwrap()) = statement.clone();
                        return result;
                    }
                }
                result.push(statement.clone());
                result
            });
        self.env.pop();

        let source = block.source.clone();
        if statements.is_empty() {
            return ExprRef::from(TypedExpr::Unit(source));
        } else if statements.len() == 1 {
            if let Some(TypedStatement::Expr(expr)) = statements.first() {
                return expr.clone_at(source);
            }
        }

        ExprRef::from(TypedExpr::Block(Block {
            statements,
            source
        }))
    }

    fn binding_stats(&mut self, binding: &BindingRef) -> &mut BindingStats {
        self.binding_stats.get_mut(&binding).unwrap()
    }

    fn fold_statement(&mut self, statement: &TypedStatement) -> TypedStatement {
        match statement {
            TypedStatement::Binding(binding) => {
                let binding = BindingRef::from(Binding {
                    name: binding.name.clone(),
                    data: self.fold(&binding.data),
                    source: binding.source.clone()
                });
                self.register_binding(binding.clone());
                TypedStatement::Binding(binding)
            },
            TypedStatement::Expr(expr) => TypedStatement::Expr(self.fold(&expr))
        }
    }

    fn register_binding(&mut self, binding: BindingRef) {
        self.env.bind(binding.name.clone(), binding.clone());
        self.binding_stats.insert(binding, BindingStats {
            usages: 0,
            assigned: false
        });
    }
}

#[cfg(test)]
macro_rules! dummy_source_file {
    () => (SourceFile::empty(format!("{}({})", file!(), line!())))
}

#[cfg(test)]
macro_rules! dummy_source {
    () => (Source::new(dummy_source_file!(), Range::new(0, 0)))
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use num_bigint::BigInt;
    use num_traits::One;

    use crate::semantics::Expression;
    use crate::semantics::ExprRef;
    use crate::semantics::FunctionType;
    use crate::semantics::FunctionTypeRef;
    use crate::semantics::Lambda;
    use crate::semantics::LambdaRef;
    use crate::semantics::Type;
    use crate::semantics::TypedExpr;
    use crate::source::Range;
    use crate::source::Source;
    use crate::source::SourceFile;

    #[test]
    fn is_primitive_constant() {
        // Incompatible types don't matter here
        let arg1 = ExprRef::from(TypedExpr::Unit(dummy_source!()));
        let arg2 = ExprRef::from(TypedExpr::Int(BigInt::one(), dummy_source!()));
        let arg3 = ExprRef::from(TypedExpr::String(Rc::new("".to_owned()), dummy_source!()));
        assert!(arg1.is_primitive_constant());
        assert!(arg2.is_primitive_constant());
        assert!(arg3.is_primitive_constant());
        assert!(!TypedExpr::AddInt(arg1.clone(), arg2.clone(), dummy_source!()).is_primitive_constant());
        let conditional = TypedExpr::Conditional {
            condition: arg1, positive: arg2, negative: Some(arg3), source: dummy_source!()
        };
        assert!(!conditional.is_primitive_constant());
    }

    #[test]
    fn is_constant() {
        let lambda = LambdaRef::from(Lambda {
            type_: FunctionTypeRef::new(FunctionType {
                parameters: Vec::new(),
                result: Type::Unit
            }),
            parameters: Vec::new(),
            body: ExprRef::from(TypedExpr::Unit(dummy_source!()))
        });
        let lambda = TypedExpr::Lambda(lambda, dummy_source!());
        assert!(lambda.is_constant());
    }
}
