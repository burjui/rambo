use crate::env::Environment;
use crate::semantics::BindingRef;
use crate::semantics::BindingValue;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use num_bigint::BigInt;
use num_traits::Zero;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

// TODO implement operation-specific optimizations, such as "x*1 = x", "x+0 = x" and so on
// TODO get rid of unreachable()

crate struct CFP {
    env: Environment<BindingRef, ExprRef>,
    current_indent_level: usize
}

macro_rules! debug_println_indented {
    ($self: ident, $format_string: expr $(, $argument: expr)*) => {{
        debug_print!("{}", "  ".repeat($self.current_indent_level));
        debug_println!($format_string $(, $argument)*);
    }};
}

impl CFP {
    crate fn new() -> CFP {
        CFP {
            env: Environment::new(),
            current_indent_level: 0
        }
    }

    crate fn fold_block(&mut self, block: &Block) -> Block {
        debug_println_indented!(self, "#BLOCK# {:?}", block);
        self.env.push();
        let statements = block.statements.iter()
            .map(|statement| {
                self.current_indent_level += 1;
                let statement = self.fold_statement(statement);
                self.current_indent_level -= 1;
                statement
            })
            .collect::<Vec<_>>();
        self.env.pop();

        let statements = statements.iter()
            .fold(Vec::new(), |mut result, statement| {
                debug_println_indented!(self, ">> {:?}", statement);
                if let (Some(TypedStatement::Expr(e1)), TypedStatement::Expr(e2)) = (result.last(), &statement) {
                    debug_println_indented!(self, "## {:?}", e1);
                    debug_println_indented!(self, "## {:?}\n", e2);
                    if is_primitive_constant(e1) && is_primitive_constant(e2) {
                        (*result.last_mut().unwrap()) = statement.clone();
                    }
                } else {
                    result.push(statement.clone());
                }
                result
            });
        Block {
            statements,
            source: block.source.clone()
        }
    }

    fn fold_statement(&mut self, statement: &TypedStatement) -> TypedStatement {
        match statement {
            TypedStatement::Binding(binding) => {
                self.process_binding(binding);
                TypedStatement::Binding(binding.clone())
            },
            TypedStatement::Expr(expr) => TypedStatement::Expr(self.fold(&expr))
        }
    }

    #[must_use]
    fn fold(&mut self, expr: &ExprRef) -> ExprRef {
        match expr as &TypedExpr {
            TypedExpr::Deref(binding, source) => {
                match &binding.borrow().data {
                    BindingValue::Var(value) => {
                        let value = self.fold(value);
                        if is_primitive_constant(&value) {
                            value.clone_at(source.clone())
                        } else {
                            expr.clone()
                        }
                    },
                    BindingValue::Arg(_) => {
                        let value = self.env.resolve(binding).unwrap().clone();
                        let value = self.fold(&value);
                        debug_println_indented!(self, "#DEREF ARG# {:?} -> {:?}", binding, value);
                        value
                    }
                }
            }
            TypedExpr::AddInt(left, right, source) => self.try_fold_numeric(expr, left, right, Add::add, source),
            TypedExpr::SubInt(left, right, source) => self.try_fold_numeric(expr, left, right, Sub::sub, source),
            TypedExpr::MulInt(left, right, source) => self.try_fold_numeric(expr, left, right, Mul::mul, source),
            TypedExpr::DivInt(left, right, source) => self.try_fold_numeric(expr, left, right, Div::div, source),
            TypedExpr::AddStr(left, right, source) => {
                match (&self.fold(left) as &TypedExpr, &self.fold(right) as &TypedExpr) {
                    (TypedExpr::String(left, _), TypedExpr::String(right, _)) =>
                        ExprRef::from(TypedExpr::String(left.to_string() + right, source.clone())),
                    _ => expr.clone()
                }
            },
            TypedExpr::Application { function, arguments, source, .. } => {
                debug_println_indented!(self, "#APPL# {:?}", function);
                let original_function = function;
                let mut function = self.fold(original_function);
                debug_println_indented!(self, "#FOLDED# {:?}", function);
                if is_primitive_constant(&function) {
                    debug_println_indented!(self, "#FN=CONST#");
                    function
                } else {
                    debug_println_indented!(self, "#FN!=CONST#");
                    while let TypedExpr::Deref(binding, _) = &function.clone() as &TypedExpr {
                        function = match &binding.borrow().data {
                            BindingValue::Var(value) => value.clone(),
                            _ => unreachable!()
                        };
                    }
                    if let TypedExpr::Lambda(lambda, _) = &function as &TypedExpr {
                        self.env.push();
                        for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
                            debug_println_indented!(self, "#PARAM# {:?}", parameter);
                            let old_argument = argument;
                            let argument = self.fold(argument);
                            debug_println_indented!(self, "#ARG# {:?} -> {:?}\n", old_argument, argument);
                            self.env.bind(parameter.clone(), argument.clone()).unwrap();
                        }
                        let result = self.fold(&lambda.body);
                        debug_println_indented!(self, "#APPL-RESULT# {:?}\n", result);
                        self.env.pop();
                        if is_primitive_constant(&result) {
                            return result.clone_at(source.clone())
                        }
                    }
                    expr.clone()
                }
            },
            TypedExpr::Assign(binding, value, source) => {
                binding.borrow_mut().assigned = true;
                ExprRef::from(TypedExpr::Assign(binding.clone(), self.fold(value), source.clone()))
            },
            TypedExpr::Lambda(lambda, source) => {
                debug_println_indented!(self, "#LAMBDA# {:?}", lambda);
                self.env.push();
                for parameter in &lambda.parameters {
                    self.env.bind(parameter.clone(), ExprRef::from(TypedExpr::Phantom)).unwrap();
                }
                let body = self.fold(&lambda.body);
                self.env.pop();
                if is_primitive_constant(&body) {
                    body.clone_at(source.clone())
                } else {
                    expr.clone()
                }
            },
            TypedExpr::Conditional { condition, positive, negative, source } => {
                let condition = self.fold(condition);
                if let TypedExpr::Int(n, _) = &condition as &TypedExpr {
                    return if n == &BigInt::zero() {
                        negative.as_ref()
                            .map(|clause| self.fold(clause))
                            .unwrap_or_else(|| ExprRef::from(TypedExpr::Unit(source.clone())))
                    } else {
                        self.fold(positive).clone_at(source.clone())
                    }
                }
                expr.clone()
            },
            TypedExpr::Block(block) => ExprRef::from(TypedExpr::Block(self.fold_block(block))),
            TypedExpr::Phantom |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => expr.clone()
        }
    }

    fn process_binding(&mut self, binding: &BindingRef) {
        let value = binding.borrow().data.clone();
        if let BindingValue::Var(value) = value {
            if !binding.borrow().assigned {
                let mut value = value.clone();
                if !binding.borrow().dirty {
                    let mut binding = binding.borrow_mut();
                    binding.dirty = true;
                    value = self.fold(&value);
                    binding.data = BindingValue::Var(value.clone());
                }
            }
        }
    }

    #[must_use]
    fn try_fold_numeric<FoldFn>(&mut self, original_expr: &ExprRef,
        left: &ExprRef, right: &ExprRef, fold_impl: FoldFn, source: &Source) -> ExprRef
        where FoldFn: FnOnce(BigInt, BigInt) -> BigInt
    {
        let (left, right) = (self.fold(left), self.fold(right));
        debug_println_indented!(self, "#ARITH# {:?}, {:?}: {:?}", left, right, source);
        match (&left as &TypedExpr, &right as &TypedExpr) {
            (TypedExpr::Int(left, _), TypedExpr::Int(right, _)) => {
                let result = fold_impl(left.clone(), right.clone());
                ExprRef::from(TypedExpr::Int(result, source.clone()))
            },
            _ => original_expr.clone()
        }
    }
}

fn is_primitive_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        &TypedExpr::Unit(_) | TypedExpr::Int(_, _) | &TypedExpr::String(_, _) => true,
        _ => false
    }
}
