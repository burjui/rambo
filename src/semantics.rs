use std::error::Error;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::mem::replace;
use std::ops::Deref;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;

use crate::env::Environment;
use crate::parser::BinaryOperation;
use crate::parser::Block as ASTBlock;
use crate::parser::Expr;
use crate::parser::Parameter;
use crate::parser::Statement;
use crate::source::Source;
use crate::unique_rc::UniqueRc;

pub(crate) struct SemanticsChecker {
    env: Environment<Rc<String>, BindingRef>,
}

impl SemanticsChecker {
    pub(crate) fn new() -> Self {
        SemanticsChecker {
            env: Environment::new(),
        }
    }

    pub(crate) fn check_module(mut self, code: &ASTBlock) -> CheckResult<ExprRef> {
        let block = self.check_block(code.statements.iter(), code.source.clone())?;
        Ok(self.new_expr(block))
    }

    fn check_statement(&mut self, statement: &Statement) -> CheckResult<TypedStatement> {
        match statement {
            Statement::Expr(expr) => {
                let expr = self.check_expr(expr)?;
                Ok(TypedStatement::Expr(expr))
            },
            Statement::Binding { name, value, source } => {
                let name = Rc::new(name.text().to_owned());
                let value = self.check_expr(&value)?;
                let binding = BindingRef::from(Binding::new(name.clone(), value, source.clone()));
                self.env.bind(name, binding.clone());
                Ok(TypedStatement::Binding(binding))
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> CheckResult<ExprRef> {
        match expr {
            Expr::Unit(source) => Ok(self.new_expr(TypedExpr::Unit(source.clone()))),
            Expr::Int(source) => {
                let value = source.text().parse::<BigInt>()?;
                Ok(self.new_expr(TypedExpr::Int(value, source.clone())))
            },
            Expr::String(source) => {
                let text = source.text();
                let value = Rc::new(text[1..text.len() - 1].to_owned());
                Ok(self.new_expr(TypedExpr::String(value, source.clone())))
            },
            Expr::Id(name) => {
                let binding = self.resolve(name)?;
                Ok(self.new_expr(TypedExpr::Reference(binding.clone(), name.clone())))
            },
            Expr::Binary { operation, left, right, .. } => {
                match operation {
                    BinaryOperation::Assign => {
                        if let Expr::Id(name) = left as &Expr {
                            let binding = self.resolve(name)?;
                            let value = self.check_expr(right)?;
                            let binding_type = binding.data.type_();
                            let value_type = value.type_();
                            if binding.data.type_() == value.type_() {
                                Ok(self.new_expr(TypedExpr::Assign(binding, value, expr.source().clone())))
                            } else {
                                error!("assigning a value of type `{:?}' to the variable of type `{:?}': {:?}", value_type, binding_type, &expr)
                            }
                        } else {
                            error!("a variable expected at the left side of assignment, but found: {:?}", left)
                        }
                    },
                    _ => {
                        let left_checked = self.check_expr(left)?;
                        let right_checked = self.check_expr(right)?;
                        let left_type = left_checked.type_();
                        let right_type = right_checked.type_();
                        if left_type != right_type {
                            return error!("operands of `{:?}' have incompatible types:\n  {:?}: {:?}\n  {:?}: {:?}",
                                          operation, left, left_type, right, right_type);
                        }

                        match operation {
                            BinaryOperation::Add => match left_type {
                                Type::Int => Ok(self.new_expr(TypedExpr::AddInt(left_checked, right_checked, expr.source().clone()))),
                                Type::String => Ok(self.new_expr(TypedExpr::AddStr(left_checked, right_checked, expr.source().clone()))),
                                _ => error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                            },
                            operation => {
                                if let Type::Int = left_type {
                                    let constructor =
                                        match operation {
                                            BinaryOperation::Subtract => TypedExpr::SubInt,
                                            BinaryOperation::Multiply => TypedExpr::MulInt,
                                            BinaryOperation::Divide => TypedExpr::DivInt,
                                            _ => unreachable!()
                                        };
                                    Ok(self.new_expr(constructor(left_checked, right_checked, expr.source().clone())))
                                } else {
                                    error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                                }
                            }
                        }
                    }
                }
            },
            Expr::Lambda { source, parameters, body } => {
                let function = self.check_function(parameters.as_slice(), &body)?;
                Ok(self.new_expr(TypedExpr::Lambda(LambdaRef::from(function), source.clone())))
            },
            Expr::Application { source, function, arguments } => {
                let function = self.check_expr(&function)?;
                let function_type = function.type_();
                let function_type_result: CheckResult<FunctionTypeRef> = match function_type {
                    Type::Function(type_) => Ok(type_.clone()),
                    _ => error!("2 expected a function, found `{:?}' of type `{:?}'", expr, function_type)
                };
                let function_type = function_type_result?;

                let arguments_checked = {
                    let function_parameters = &function_type.parameters;
                    let parameter_count = function_parameters.len();
                    let argument_count = arguments.len();
                    if argument_count != parameter_count {
                        return error!("{}: invalid number of arguments: expected {}, found {}:\n  {:?}",
                                      source, parameter_count, argument_count, expr);
                    }

                    let mut arguments_checked = vec![];
                    for (parameter, argument) in function_parameters.iter().zip(arguments.iter()) {
                        let argument_checked = self.check_expr(argument)?;
                        let argument_type = argument_checked.type_();
                        if argument_type != parameter.type_ {
                            return error!("argument type mismatch for parameter `{}': expected a `{:?}', found `{:?}` of type `{:?}'",
                                          parameter.name.text(), parameter.type_, argument, argument_type);
                        }
                        arguments_checked.push(argument_checked);
                    }

                    arguments_checked
                };

                Ok(self.new_expr(TypedExpr::Application {
                    type_: function_type.result.clone(),
                    function,
                    arguments: arguments_checked,
                    source: source.clone()
                }))
            },
            Expr::Conditional { source, condition, true_branch, false_branch } => {
                let condition_typed = self.check_expr(condition)?;
                let condition_type = condition_typed.type_();
                if condition_type != Type::Int {
                    return error!("a condition can only be of type `num': {:?}", condition.source())
                }

                let (true_branch_statements, true_branch_source) = match true_branch.deref() {
                    Expr::Block(ASTBlock { source, statements }) => (statements, source),
                    _ => unreachable!()
                };
                let true_branch = if true_branch_statements.is_empty() {
                    warning!("empty true branch: {:?}", true_branch_source);
                    TypedExpr::Unit(true_branch_source.clone())
                } else {
                    let additional_statement = if false_branch.is_none() {
                        vec![Statement::Expr(Expr::Unit(true_branch_source.clone()))]
                    } else {
                        vec![]
                    };
                    let statements = true_branch_statements.iter().chain(additional_statement.iter());
                    self.check_block(statements, true_branch_source.clone())?
                };
                let true_branch_type = true_branch.type_();

                let false_branch = match false_branch {
                    Some(false_branch) => {
                        let (false_branch_statements, false_branch_source) = match false_branch.deref() {
                            Expr::Block(ASTBlock { source, statements }) => (statements, source),
                            _ => unreachable!()
                        };
                        if false_branch_statements.is_empty() {
                            warning!("empty false branch: {:?}", false_branch_source);
                            TypedExpr::Unit(false_branch_source.clone())
                        } else {
                            self.check_block(false_branch_statements.iter(), false_branch_source.clone())?
                        }
                    }

                    None => TypedExpr::Unit(source.clone())
                };
                let false_branch_type = false_branch.type_();

                if true_branch_type != false_branch_type {
                    return error!("branch types don't match: `{:?}' and `{:?}'",
                                  true_branch_type, false_branch_type);
                }

                Ok(self.new_expr(TypedExpr::Conditional {
                    condition: condition_typed,
                    true_branch: self.new_expr(true_branch),
                    false_branch: self.new_expr(false_branch),
                    type_: true_branch_type,
                    source: source.clone()
                }))
            },
            Expr::Block(ASTBlock { source, statements }) => {
                let statements = statements.iter()
                    .map(|statement| self.check_statement(statement))
                    .collect::<CheckResult<Vec<_>>>()?;
                Ok(self.new_expr(TypedExpr::Block(statements, source.clone())))
            }
        }
    }

    fn check_function(&mut self, parameters: &[Parameter], body: &Expr) -> CheckResult<Lambda> {
        let outer_env = replace(&mut self.env, Environment::new());
        self.env.push();
        let mut parameter_bindings = vec![];
        for parameter in parameters {
            let name = Rc::new(parameter.name.text().to_owned());
            let value = self.new_expr(TypedExpr::ArgumentPlaceholder(name.clone(), parameter.type_.clone()));
            let binding = BindingRef::from(Binding::new(name.clone(), value, parameter.source.clone()));
            self.env.bind(name.clone(), binding.clone());
            parameter_bindings.push(binding);
        }
        let body = self.check_expr(body)?;
        self.env = outer_env; // no need for self.env.pop()

        let function_type = Rc::new(FunctionType {
            parameters: parameters.to_vec(),
            result: body.type_()
        });
        Ok(Lambda {
            type_: function_type,
            parameters: parameter_bindings,
            body
        })
    }

    fn check_block<'a, BlockIterator>(&mut self, block: BlockIterator, source: Source) -> CheckResult<TypedExpr>
        where BlockIterator: Iterator<Item = &'a Statement>
    {
        self.env.push();
        let mut statements: Vec<_> = block
            .map(|statement| self.check_statement(statement))
            .collect::<Result<_, _>>()?;
        self.env.pop();

        let expr = if statements.is_empty() {
            TypedExpr::Unit(source.clone())
        }
        else {
            if let TypedStatement::Binding(_) = statements.last().unwrap() {
                statements.push(TypedStatement::Expr(self.new_expr(TypedExpr::Unit(source.clone()))));
            }
            TypedExpr::Block(statements, source)
        };
        Ok(expr)
    }

    fn new_expr(&self, expr: TypedExpr) -> ExprRef {
        ExprRef(Rc::new(expr))
    }

    fn resolve(&self, name: &Source) -> Result<BindingRef, String> {
        self.env
            .resolve(&Rc::new(name.text().to_owned()))
            .map_err(|message| format!("{}: {}", name, message))
            .map(Clone::clone)
    }
}

type CheckResult<T> = Result<T, Box<dyn Error>>;
pub(crate) type FunctionTypeRef = Rc<FunctionType>;

#[derive(Clone, PartialEq)]
pub(crate) struct FunctionType {
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) result: Type
}

impl Debug for FunctionType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let parameter_list = self.parameters.iter()
            .map(|parameter| format!("{}: {:?}", parameter.name.text(), parameter.type_))
            .format(", ");
        write!(formatter, "λ ({}) -> {:?}", parameter_list, self.result)
    }
}

#[derive(Clone, PartialEq)]
pub(crate) enum Type {
    Unit,
    Int,
    String,
    Function(FunctionTypeRef),
}

impl Debug for Type {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(formatter, "()"),
            Type::Int => write!(formatter, "num"),
            Type::String => write!(formatter, "str"),
            Type::Function(type_) => type_.fmt(formatter)
        }
    }
}

#[derive(Clone)]
pub(crate) struct ExprRef(Rc<TypedExpr>);

impl Deref for ExprRef {
    type Target = TypedExpr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Debug for ExprRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}

#[derive(Clone)]
pub(crate) struct Lambda {
    pub(crate) type_: FunctionTypeRef,
    pub(crate) parameters: Vec<BindingRef>,
    pub(crate) body: ExprRef
}

impl Debug for Lambda {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "(λ {:?} = {:?})", self.type_, &self.body)
    }
}

pub(crate) enum TypedExpr {
    ArgumentPlaceholder(Rc<String>, Type),
    Unit(Source),
    Int(BigInt, Source),
    String(Rc<String>, Source),
    Reference(BindingRef, Source),
    Lambda(LambdaRef, Source),
    Application {
        type_: Type,
        function: ExprRef,
        arguments: Vec<ExprRef>,
        source: Source
    },
    AddInt(ExprRef, ExprRef, Source),
    SubInt(ExprRef, ExprRef, Source),
    MulInt(ExprRef, ExprRef, Source),
    DivInt(ExprRef, ExprRef, Source),
    AddStr(ExprRef, ExprRef, Source),
    Assign(BindingRef, ExprRef, Source),
    Conditional {
        condition: ExprRef,
        true_branch: ExprRef,
        false_branch: ExprRef,
        type_: Type,
        source: Source
    },
    Block(Vec<TypedStatement>, Source)
}

impl Debug for TypedExpr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExpr::ArgumentPlaceholder(name, _) => write!(formatter, "<{}>", name),
            TypedExpr::Unit(_) => write!(formatter, "()"),
            TypedExpr::Int(value, _) => write!(formatter, "{}", value),
            TypedExpr::String(value, _) => write!(formatter, "\"{}\"", value),
            TypedExpr::Reference(binding, _) => write!(formatter, "&{}", &binding.name),
            TypedExpr::AddInt(left, right, _) => write!(formatter, "({:?} + {:?})", left, right),
            TypedExpr::SubInt(left, right, _) => write!(formatter, "({:?} - {:?})", left, right),
            TypedExpr::MulInt(left, right, _) => write!(formatter, "({:?} * {:?})", left, right),
            TypedExpr::DivInt(left, right, _) => write!(formatter, "({:?} / {:?})", left, right),
            TypedExpr::AddStr(left, right, _) => write!(formatter, "({:?} + {:?})", left, right),
            TypedExpr::Assign(binding, value, _) => write!(formatter, "({} = {:?})", &binding.name, value),
            TypedExpr::Lambda(lambda, _) => lambda.fmt(formatter),
            TypedExpr::Application { function, arguments, .. } => write!(formatter, "({:?} @ {:?})", function, arguments),
            TypedExpr::Conditional { condition, true_branch, false_branch, .. } =>
                write!(formatter, "(if ({:?}) {:?} else {:?})", condition, true_branch, false_branch),
            TypedExpr::Block(statements, _) => write!(formatter, "{{\n{:?}\n}}", statements.iter().format("\n")),
        }
    }
}

impl TypedExpr {
    pub(crate) fn type_(&self) -> Type {
        match self {
            TypedExpr::ArgumentPlaceholder(_, type_) => type_.clone(),
            TypedExpr::Unit(_) => Type::Unit,

            TypedExpr::Int(_, _) |
            TypedExpr::AddInt(_, _, _) |
            TypedExpr::SubInt(_, _, _) |
            TypedExpr::MulInt(_, _, _) |
            TypedExpr::DivInt(_, _, _) => Type::Int,

            TypedExpr::String(_, _) |
            TypedExpr::AddStr(_, _, _) => Type::String,

            TypedExpr::Reference(binding, _) => binding.data.type_(),
            TypedExpr::Assign(_, value, _) => value.type_(),
            TypedExpr::Lambda(lambda, _) => Type::Function(lambda.type_.clone()),
            TypedExpr::Application { type_, .. } => type_.clone(),
            TypedExpr::Conditional { type_, .. } => type_.clone(),

            TypedExpr::Block(statements, _) => statements.last()
                .map(TypedStatement::type_)
                .unwrap_or_else(|| Type::Unit)
        }
    }
}

pub(crate) type LambdaRef = UniqueRc<Lambda>;
pub(crate) type BindingRef = UniqueRc<Binding>;

pub(crate) struct Binding {
    pub(crate) name: Rc<String>,
    pub(crate) data: ExprRef,
    pub(crate) source: Source,
}

impl Binding {
    pub(crate) fn new(name: Rc<String>, data: ExprRef, source: Source) -> Binding {
        Binding { name, data, source }
    }
}

impl Debug for Binding {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "let {} = {:?}", self.name, self.data)
    }
}

#[derive(Clone)]
pub(crate) enum TypedStatement {
    Expr(ExprRef),
    Binding(BindingRef)
}

impl Debug for TypedStatement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedStatement::Expr(expr) => expr.fmt(formatter),
            TypedStatement::Binding(binding) => binding.fmt(formatter)
        }
    }
}

impl TypedStatement {
    pub(crate) fn type_(&self) -> Type {
        match self {
            TypedStatement::Binding(_) => Type::Unit,
            TypedStatement::Expr(expr) => expr.type_()
        }
    }
}
