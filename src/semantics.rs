use core::ops::RangeFrom;
use std::{
    error::Error,
    fmt::{Debug, Formatter},
    mem::replace,
    ops::Deref,
    rc::Rc,
};

use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    env::Environment,
    parser::{BinaryOperation, Block as ASTBlock, Expr, Parameter, Statement},
    source::Source,
    unique_rc::StaticRef,
};

pub(crate) struct EnableWarnings(pub(crate) bool);

pub(crate) struct SemanticsChecker {
    enable_warnings: bool,
    env: Environment<Rc<String>, BindingRef>,
    function_ids: RangeFrom<usize>,
    binding_is_used: FxHashMap<BindingRef, bool>,
}

impl SemanticsChecker {
    pub(crate) fn new(EnableWarnings(enable_warnings): EnableWarnings) -> Self {
        Self {
            enable_warnings,
            env: Environment::new(),
            function_ids: 0..,
            binding_is_used: FxHashMap::default(),
        }
    }

    pub(crate) fn check_module(mut self, code: &ASTBlock) -> CheckResult<ExprRef> {
        let block = self.check_block(code.statements.iter(), code.source.clone())?;
        if self.enable_warnings {
            for unused_binding in self
                .binding_is_used
                .into_iter()
                .filter_map(|(binding, is_used)| if is_used { None } else { Some(binding) })
                .sorted_by_key(|binding| binding.source.range().start)
            {
                warning_at!(
                    unused_binding.source,
                    "unused definition: {}",
                    unused_binding.source.text()
                );
            }
        }

        let source = match &block {
            TypedExpr::Block(_, source) => source.clone(),
            _ => unreachable!("not a block: {:?}", block),
        };
        let name = Rc::new("main".to_string());
        let function = FunctionRef::from(Function {
            name: name.clone(),
            type_: Rc::new(FunctionType {
                parameters: Vec::new(),
                result: Type::Unit,
            }),
            parameters: Vec::new(),
            body: new_expr(block),
        });
        let main_binding = BindingRef::from(Binding {
            name,
            data: new_expr(TypedExpr::Function(function, source.clone())),
            source: source.clone(),
        });
        let program = vec![
            TypedStatement::Binding(main_binding.clone()),
            TypedStatement::Expr(new_expr(TypedExpr::Application {
                type_: Type::Unit,
                function: new_expr(TypedExpr::Reference(main_binding, source.clone())),
                arguments: vec![],
                source: source.clone(),
            })),
        ];
        Ok(new_expr(TypedExpr::Block(program, source)))
    }

    fn check_statement(&mut self, statement: &Statement) -> CheckResult<TypedStatement> {
        match statement {
            Statement::Expr(expr) => {
                let expr = self.check_expr(expr)?;
                Ok(TypedStatement::Expr(expr))
            }
            Statement::Binding {
                name,
                value,
                source,
            } => {
                let name = Rc::new(name.text().to_owned());
                let value = self.check_expr(value)?;
                let binding = BindingRef::from(Binding::new(name.clone(), value, source.clone()));
                if self.enable_warnings {
                    self.binding_is_used.insert(binding.clone(), false);
                }
                self.env.bind(name, binding.clone());
                Ok(TypedStatement::Binding(binding))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn check_expr(&mut self, expr: &Expr) -> CheckResult<ExprRef> {
        match expr {
            Expr::Unit(source) => Ok(new_expr(TypedExpr::Unit(source.clone()))),
            Expr::Int(source) => match source.text().parse::<i32>() {
                Ok(value) => Ok(new_expr(TypedExpr::Int(value, source.clone()))),
                Err(_) => error!(
                    "{}: number `{}' is out of 32-bit signed integer range",
                    source,
                    source.text()
                ),
            },
            Expr::String(source) => {
                let text = source.text();
                let value = Rc::new(text[1..text.len() - 1].to_owned());
                Ok(new_expr(TypedExpr::String(value, source.clone())))
            }
            Expr::Id(name) => {
                let binding = self.resolve(name)?;
                if self.enable_warnings {
                    *self.binding_is_used.get_mut(&binding).unwrap() = true;
                }
                Ok(new_expr(TypedExpr::Reference(binding, name.clone())))
            }
            Expr::Binary {
                operation,
                left,
                right,
                ..
            } => {
                if let BinaryOperation::Assign = operation {
                    if let Expr::Id(name) = left as &Expr {
                        let binding = self.resolve(name)?;
                        let value = self.check_expr(right)?;
                        let binding_type = binding.data.type_();
                        let value_type = value.type_();
                        if binding.data.type_() == value.type_() {
                            Ok(new_expr(TypedExpr::Assign(
                                binding,
                                value,
                                expr.source().clone(),
                            )))
                        } else {
                            error!(
                                "assigning a value of type `{:?}' to the variable of type `{:?}': {:?}",
                                value_type, binding_type, &expr
                            )
                        }
                    } else {
                        error!(
                            "a variable expected at the left side of assignment, but found: {:?}",
                            left
                        )
                    }
                } else {
                    let left_checked = self.check_expr(left)?;
                    let right_checked = self.check_expr(right)?;
                    let left_type = left_checked.type_();
                    let right_type = right_checked.type_();
                    if left_type != right_type {
                        return error!(
                            "operands of `{:?}' have incompatible types:\n  {:?}: {:?}\n  {:?}: {:?}",
                            operation, left, left_type, right, right_type
                        );
                    }

                    match operation {
                        BinaryOperation::Add => match left_type {
                            Type::Int => Ok(new_expr(TypedExpr::AddInt(
                                left_checked,
                                right_checked,
                                expr.source().clone(),
                            ))),
                            Type::String => Ok(new_expr(TypedExpr::AddStr(
                                left_checked,
                                right_checked,
                                expr.source().clone(),
                            ))),
                            _ => error!(
                                "operation `{:?}' is not implemented for type `{:?}'",
                                operation, left_type
                            ),
                        },
                        operation => {
                            if let Type::Int = left_type {
                                let constructor = match operation {
                                    BinaryOperation::Subtract => TypedExpr::SubInt,
                                    BinaryOperation::Multiply => TypedExpr::MulInt,
                                    BinaryOperation::Divide => TypedExpr::DivInt,
                                    _ => unreachable!(),
                                };
                                Ok(new_expr(constructor(
                                    left_checked,
                                    right_checked,
                                    expr.source().clone(),
                                )))
                            } else {
                                error!(
                                    "operation `{:?}' is not implemented for type `{:?}'",
                                    operation, left_type
                                )
                            }
                        }
                    }
                }
            }
            Expr::Function {
                name,
                source,
                parameters,
                body,
            } => {
                let name = name.as_ref().map_or_else(
                    || self.generate_function_name(),
                    |name| name.text().to_owned(),
                );
                let function = self.check_function(name, parameters.as_slice(), body)?;
                Ok(new_expr(TypedExpr::Function(
                    FunctionRef::from(function),
                    source.clone(),
                )))
            }
            Expr::Application {
                source,
                function,
                arguments,
            } => {
                let function = self.check_expr(function)?;
                let function_type = function.type_();
                let function_type_result: CheckResult<FunctionTypeRef> = match function_type {
                    Type::Function(type_) => Ok(type_),
                    _ => error!(
                        "2 expected a function, found `{:?}' of type `{:?}'",
                        expr, function_type
                    ),
                };
                let function_type = function_type_result?;

                let arguments_checked = {
                    let function_parameters = &function_type.parameters;
                    let parameter_count = function_parameters.len();
                    let argument_count = arguments.len();
                    if argument_count != parameter_count {
                        return error!(
                            "{}: invalid number of arguments: expected {}, found {}:\n  {:?}",
                            source, parameter_count, argument_count, expr
                        );
                    }

                    let mut arguments_checked = vec![];
                    for (parameter, argument) in function_parameters.iter().zip(arguments.iter()) {
                        let checked = self.check_expr(argument)?;
                        let checked_type = checked.type_();
                        if checked_type != parameter.type_ {
                            return error!("argument type mismatch for parameter `{}': expected a `{:?}', found `{:?}` of type `{:?}'",
                                          parameter.name.text(), parameter.type_, argument, checked_type);
                        }
                        arguments_checked.push(checked);
                    }

                    arguments_checked
                };

                Ok(new_expr(TypedExpr::Application {
                    type_: function_type.result.clone(),
                    function,
                    arguments: arguments_checked,
                    source: source.clone(),
                }))
            }
            Expr::Conditional {
                source,
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_typed = self.check_expr(condition)?;
                let condition_type = condition_typed.type_();
                if condition_type != Type::Int {
                    return error!(
                        "a condition can only be of type `num': {:?}",
                        condition.source()
                    );
                }

                let Expr::Block(ASTBlock {
                    source: then_branch_source,
                    statements: then_branch_statements,
                }) = &**then_branch
                else {
                    unreachable!()
                };
                let then_branch = if then_branch_statements.is_empty() {
                    warning!("empty true branch: {:?}", then_branch_source);
                    TypedExpr::Unit(then_branch_source.clone())
                } else {
                    let additional_statement = if else_branch.is_none() {
                        vec![Statement::Expr(Expr::Unit(then_branch_source.clone()))]
                    } else {
                        vec![]
                    };
                    let statements = then_branch_statements
                        .iter()
                        .chain(additional_statement.iter());
                    self.check_block(statements, then_branch_source.clone())?
                };
                let then_branch_type = then_branch.type_();

                let else_branch = match else_branch {
                    Some(else_branch) => {
                        let Expr::Block(ASTBlock {
                            source: else_branch_source,
                            statements: else_branch_statements,
                        }) = &**else_branch
                        else {
                            unreachable!()
                        };
                        if else_branch_statements.is_empty() {
                            warning!("empty false branch: {:?}", else_branch_source);
                            TypedExpr::Unit(else_branch_source.clone())
                        } else {
                            self.check_block(
                                else_branch_statements.iter(),
                                else_branch_source.clone(),
                            )?
                        }
                    }

                    None => TypedExpr::Unit(source.clone()),
                };
                let else_branch_type = else_branch.type_();

                if then_branch_type != else_branch_type {
                    return error!(
                        "branch types don't match: `{:?}' and `{:?}'",
                        then_branch_type, else_branch_type
                    );
                }

                Ok(new_expr(TypedExpr::Conditional {
                    condition: condition_typed,
                    then_branch: new_expr(then_branch),
                    else_branch: new_expr(else_branch),
                    type_: then_branch_type,
                    source: source.clone(),
                }))
            }
            Expr::Block(ASTBlock { source, statements }) => {
                let statements = statements
                    .iter()
                    .map(|statement| self.check_statement(statement))
                    .collect::<CheckResult<Vec<_>>>()?;
                Ok(new_expr(TypedExpr::Block(statements, source.clone())))
            }
        }
    }

    fn check_function(
        &mut self,
        name: String,
        parameters: &[Parameter],
        body: &Expr,
    ) -> CheckResult<Function> {
        let outer_env = replace(&mut self.env, Environment::new());
        self.env.push();
        let mut parameter_bindings = vec![];
        for parameter in parameters {
            let name = Rc::new(parameter.name.text().to_owned());
            let value = new_expr(TypedExpr::ArgumentPlaceholder(
                name.clone(),
                parameter.type_.clone(),
            ));
            let binding =
                BindingRef::from(Binding::new(name.clone(), value, parameter.source.clone()));
            if self.enable_warnings {
                self.binding_is_used.insert(binding.clone(), false);
            }
            self.env.bind(name.clone(), binding.clone());
            parameter_bindings.push(binding);
        }
        let body = self.check_expr(body)?;
        self.env = outer_env; // no need for self.env.pop()

        let function_type = Rc::new(FunctionType {
            parameters: parameters.to_vec(),
            result: body.type_(),
        });
        Ok(Function {
            name: Rc::new(name),
            type_: function_type,
            parameters: parameter_bindings,
            body,
        })
    }

    fn check_block<'a, BlockIterator>(
        &mut self,
        block: BlockIterator,
        source: Source,
    ) -> CheckResult<TypedExpr>
    where
        BlockIterator: Iterator<Item = &'a Statement>,
    {
        self.env.push();
        let mut statements: Vec<_> = block
            .map(|statement| self.check_statement(statement))
            .collect::<Result<_, _>>()?;
        self.env.pop();

        let expr = if statements.is_empty() {
            TypedExpr::Unit(source)
        } else {
            if let TypedStatement::Binding(_) = statements.last().unwrap() {
                statements.push(TypedStatement::Expr(new_expr(TypedExpr::Unit(
                    source.clone(),
                ))));
            }
            TypedExpr::Block(statements, source)
        };
        Ok(expr)
    }

    fn resolve(&self, name: &Source) -> Result<BindingRef, String> {
        self.env
            .resolve(&Rc::new(name.text().to_owned()))
            .map_err(|message| format!("{name}: {message}"))
            .map(Clone::clone)
    }

    fn generate_function_name(&mut self) -> String {
        let function_id = self.function_ids.next().unwrap();
        format!("@lambda{function_id}")
    }
}

type CheckResult<T> = Result<T, Box<dyn Error>>;
pub(crate) type FunctionTypeRef = Rc<FunctionType>;

#[derive(Clone, PartialEq)]
pub(crate) struct FunctionType {
    pub(crate) parameters: Vec<Parameter>,
    pub(crate) result: Type,
}

impl Debug for FunctionType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let parameter_list = self
            .parameters
            .iter()
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
            Self::Unit => write!(formatter, "()"),
            Self::Int => write!(formatter, "num"),
            Self::String => write!(formatter, "str"),
            Self::Function(type_) => type_.fmt(formatter),
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
pub(crate) struct Function {
    pub(crate) name: Rc<String>,
    pub(crate) type_: FunctionTypeRef,
    pub(crate) parameters: Vec<BindingRef>,
    pub(crate) body: ExprRef,
}

impl Debug for Function {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "({:?} = {:?})", self.type_, &self.body)
    }
}

pub(crate) enum TypedExpr {
    ArgumentPlaceholder(Rc<String>, Type),
    Unit(Source),
    Int(i32, Source),
    String(Rc<String>, Source),
    Reference(BindingRef, Source),
    Function(FunctionRef, Source),
    Application {
        type_: Type,
        function: ExprRef,
        arguments: Vec<ExprRef>,
        source: Source,
    },
    AddInt(ExprRef, ExprRef, Source),
    SubInt(ExprRef, ExprRef, Source),
    MulInt(ExprRef, ExprRef, Source),
    DivInt(ExprRef, ExprRef, Source),
    AddStr(ExprRef, ExprRef, Source),
    Assign(BindingRef, ExprRef, Source),
    Conditional {
        condition: ExprRef,
        then_branch: ExprRef,
        else_branch: ExprRef,
        type_: Type,
        source: Source,
    },
    Block(Vec<TypedStatement>, Source),
}

impl Debug for TypedExpr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ArgumentPlaceholder(name, _) => write!(formatter, "<{name}>"),
            Self::Unit(_) => write!(formatter, "()"),
            Self::Int(value, _) => write!(formatter, "{value}"),
            Self::String(value, _) => write!(formatter, "\"{value}\""),
            Self::Reference(binding, _) => write!(formatter, "&{}", &binding.name),
            Self::AddInt(left, right, _) | Self::AddStr(left, right, _) => {
                write!(formatter, "({left:?} + {right:?})")
            }
            Self::SubInt(left, right, _) => write!(formatter, "({left:?} - {right:?})"),
            Self::MulInt(left, right, _) => write!(formatter, "({left:?} * {right:?})"),
            Self::DivInt(left, right, _) => write!(formatter, "({left:?} / {right:?})"),
            Self::Assign(binding, value, _) => {
                write!(formatter, "({} = {:?})", &binding.name, value)
            }
            Self::Function(function, _) => function.fmt(formatter),
            Self::Application {
                function,
                arguments,
                ..
            } => write!(formatter, "({function:?} @ {arguments:?})"),
            Self::Conditional {
                condition,
                then_branch,
                else_branch,
                ..
            } => write!(
                formatter,
                "(if ({condition:?}) {then_branch:?} else {else_branch:?})"
            ),
            Self::Block(statements, _) => {
                write!(formatter, "{{\n{:?}\n}}", statements.iter().format("\n"))
            }
        }
    }
}

impl TypedExpr {
    pub(crate) fn type_(&self) -> Type {
        match self {
            Self::ArgumentPlaceholder(_, type_)
            | Self::Application { type_, .. }
            | Self::Conditional { type_, .. } => type_.clone(),

            Self::Unit(_) => Type::Unit,

            Self::Int(_, _)
            | Self::AddInt(_, _, _)
            | Self::SubInt(_, _, _)
            | Self::MulInt(_, _, _)
            | Self::DivInt(_, _, _) => Type::Int,

            Self::String(_, _) | Self::AddStr(_, _, _) => Type::String,

            Self::Reference(binding, _) => binding.data.type_(),
            Self::Assign(_, value, _) => value.type_(),
            Self::Function(function, _) => Type::Function(function.type_.clone()),

            Self::Block(statements, _) => statements
                .last()
                .map_or_else(|| Type::Unit, TypedStatement::type_),
        }
    }
}

pub(crate) type FunctionRef = StaticRef<Function>;
pub(crate) type BindingRef = StaticRef<Binding>;

pub(crate) struct Binding {
    pub(crate) name: Rc<String>,
    pub(crate) data: ExprRef,
    pub(crate) source: Source,
}

impl Binding {
    pub(crate) fn new(name: Rc<String>, data: ExprRef, source: Source) -> Self {
        Self { name, data, source }
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
    Binding(BindingRef),
}

impl Debug for TypedStatement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => expr.fmt(formatter),
            Self::Binding(binding) => binding.fmt(formatter),
        }
    }
}

impl TypedStatement {
    pub(crate) fn type_(&self) -> Type {
        match self {
            Self::Binding(_) => Type::Unit,
            Self::Expr(expr) => expr.type_(),
        }
    }
}

fn new_expr(expr: TypedExpr) -> ExprRef {
    ExprRef(Rc::new(expr))
}
