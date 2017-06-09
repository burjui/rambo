use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::error::Error;
use std::rc::Rc;
use std::cell::RefCell;
use num::BigInt;
use std::collections::HashMap;
use itertools::Itertools;

use parser::*;

pub type FunctionTypeRef = Rc<FunctionType>;

#[derive(Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_: Type
}

impl Debug for Parameter {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "({}: {:?})", self.name, self.type_)
    }
}

#[derive(Clone, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Parameter>,
    pub result: Type
}

impl Debug for FunctionType {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        let parameter_list = self.parameters.iter().map(|x| format!("{:?}", x)).join(" ");
        write!(formatter, "({} -> {:?})", parameter_list, self.result)
    }
}

// TODO impl Debug with parenthesis
#[derive(Clone, PartialEq)]
pub enum Type {
    Int,
    String,
    Function(FunctionTypeRef),
}

impl Debug for Type {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Type::Int => write!(formatter, "num"),
            &Type::String => write!(formatter, "str"),
            &Type::Function(ref type_) => type_.fmt(formatter)
        }
    }
}

pub type ExprRef = Rc<TypedExpr>;

pub enum TypedExpr {
    Int(BigInt),
    String(String),
    Deref(BindingRef),
    Lambda {
        type_: FunctionTypeRef,
        body: ExprRef
    },
    Application {
        type_: Type,
        function: ExprRef,
        arguments: Vec<ExprRef>
    },
    AddInt(ExprRef, ExprRef),
    SubInt(ExprRef, ExprRef),
    MulInt(ExprRef, ExprRef),
    DivInt(ExprRef, ExprRef),
    AddStr(ExprRef, ExprRef),
    Assign(ExprRef, ExprRef),
}

impl Debug for TypedExpr {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &TypedExpr::Int(ref value) => write!(formatter, "{}", value),
            &TypedExpr::String(ref value) => write!(formatter, "{}", value),
            &TypedExpr::Deref(ref binding) => write!(formatter, "(*{})", binding.borrow().name),
            &TypedExpr::AddInt(ref left, ref right) => write!(formatter, "({:?} + {:?})", left, right),
            &TypedExpr::SubInt(ref left, ref right) => write!(formatter, "({:?} - {:?})", left, right),
            &TypedExpr::MulInt(ref left, ref right) => write!(formatter, "({:?} * {:?})", left, right),
            &TypedExpr::DivInt(ref left, ref right) => write!(formatter, "({:?} / {:?})", left, right),
            &TypedExpr::AddStr(ref left, ref right) => write!(formatter, "({:?} + {:?})", left, right),
            &TypedExpr::Assign(ref left, ref right) => write!(formatter, "({:?} = {:?})", left, right),
            &TypedExpr::Lambda { ref type_, ref body } => write!(formatter, "(λ {:?} = {:?})", type_, body),
            &TypedExpr::Application { ref function, ref arguments, .. } => write!(formatter, "({:?} @ {:?})", function, arguments),
        }
    }
}

impl TypedExpr {
    pub fn type_(&self) -> Type {
        match self {
            &TypedExpr::Int(_) |
            &TypedExpr::AddInt(_, _) |
            &TypedExpr::SubInt(_, _) |
            &TypedExpr::MulInt(_, _) |
            &TypedExpr::DivInt(_, _) => Type::Int,

            &TypedExpr::String(_) |
            &TypedExpr::AddStr(_, _) => Type::String,

            &TypedExpr::Deref(ref binding) => binding.borrow().type_(),
            &TypedExpr::Assign(ref left, _) => left.type_(),
            &TypedExpr::Lambda { ref type_, .. } => Type::Function(type_.clone()),
            &TypedExpr::Application { ref type_, .. } => type_.clone(),
        }
    }
}

#[derive(Debug)]
pub enum BindingValue {
    Var(ExprRef),
    Arg(Type)
}

impl BindingValue {
    pub fn type_(&self) -> Type {
        match self {
            &BindingValue::Var(ref expr) => expr.type_(),
            &BindingValue::Arg(ref type_) => type_.clone()
        }
    }
}

pub type BindingRef = Rc<RefCell<Binding>>;

pub struct Binding {
    pub name: String,
    pub value: BindingValue,
    pub index: usize
}

impl Binding {
    pub fn type_(&self) -> Type {
        self.value.type_()
    }
}

impl Debug for Binding {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "let {} = {:?}", self.name, self.value)
    }
}

pub enum TypedEntity {
    Expr(ExprRef),
    Binding(BindingRef)
}

impl Debug for TypedEntity {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &TypedEntity::Expr(ref expr) => expr.fmt(formatter),
            &TypedEntity::Binding(ref binding) => binding.borrow().fmt(formatter)
        }
    }
}

// TODO check for unused binding upon exiting a scope

type CheckResult<T> = Result<T, Box<Error>>;

pub struct Semantics {
}

impl Semantics {
    pub fn new() -> Semantics {
        Semantics {}
    }

    pub fn check_module(&self, code: &[Entity]) -> CheckResult<Vec<TypedEntity>> {
        let mut typed_module = vec![];
        let scope = Scope::new(None);
        let mut binding_index = 0;
        for entity in code {
            let entity_checked = match entity {
                &Entity::Expr(ref expr) => {
                    let expr = self.check_expr(&scope, expr)?;
                    TypedEntity::Expr(expr)
                },
                &Entity::Binding { ref name, ref value } =>  {
                    let value = self.check_expr(&scope, value)?;
                    let binding = Rc::new(RefCell::new(Binding {
                        name: name.text().to_string(),
                        value: BindingValue::Var(value),
                        index: binding_index
                    }));
                    binding_index += 1;
                    scope.bind(name.text(), &binding)?;
                    TypedEntity::Binding(binding)
                }
            };
            typed_module.push(entity_checked);
        }
        Ok(typed_module)
    }

    fn check_expr(&self, scope: &ScopeRef, expr: &Expr) -> CheckResult<ExprRef> {
        match expr {
            &Expr::Int(ref source) => {
                let value = source.text().parse::<BigInt>()?;
                Ok(Rc::new(TypedExpr::Int(value)))
            },
            &Expr::String(ref source) => {
                let value = source.text().to_string();
                Ok(Rc::new(TypedExpr::String(value)))
            },
            &Expr::Id(ref name) => scope.resolve(name.text()),
            &Expr::Binary { ref operation, ref left, ref right } => {
                let left_checked = self.check_expr(scope, left)?;
                let right_checked = self.check_expr(scope, right)?;
                let left_type = left_checked.type_();
                let right_type = right_checked.type_();
                if left_type != right_type {
                    error!("operands of `{:?}' have incompatible types:\n  {:?}: {:?}\n  {:?}: {:?}",
                    operation, left, left_type, right, right_type)
                } else {
                    match operation {
                        &BinaryOperation::Assign => {
                            if let TypedExpr::Deref(_) = *left_checked {
                                Ok(Rc::new(TypedExpr::Assign(left_checked, right_checked)))
                            } else {
                                error!("a variable expected at the left side of assignment, but found: {:?}", left_checked)
                            }
                        },
                        &BinaryOperation::Add => match &left_type {
                            &Type::Int => Ok(Rc::new(TypedExpr::AddInt(left_checked, right_checked))),
                            &Type::String => Ok(Rc::new(TypedExpr::AddStr(left_checked, right_checked))),
                            _ => error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                        },
                        operation => {
                            if let &Type::Int = &left_type {
                                let constructor =
                                    match operation {
                                        &BinaryOperation::Subtract => TypedExpr::SubInt,
                                        &BinaryOperation::Multiply => TypedExpr::MulInt,
                                        &BinaryOperation::Divide => TypedExpr::DivInt,
                                        _ => unreachable!()
                                    };
                                Ok(Rc::new(constructor(left_checked, right_checked)))
                            } else {
                                error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                            }
                        }
                    }
                }
            },
            &Expr::Lambda { ref parameters, ref body } => {
                let lambda_scope = Scope::new(Some(scope.clone()));
                for (parameter_index, parameter) in parameters.iter().enumerate() {
                    let parameter_name = parameter.name.text();
                    let binding = Rc::new(RefCell::new(Binding {
                        name: parameter_name.to_string(),
                        value: BindingValue::Arg(parameter.type_.clone()),
                        index: parameter_index
                    }));
                    lambda_scope.bind(parameter_name, &binding)?;
                }
                let body = self.check_expr(&lambda_scope, body)?;
                let parameters = parameters.iter()
                    .map(|parameter| Parameter {
                        name: parameter.name.text().to_string(),
                        type_: parameter.type_.clone()
                    })
                    .collect();
                let function_type = Rc::new(FunctionType {
                    parameters,
                    result: body.type_()
                });
                Ok(Rc::new(TypedExpr::Lambda{
                    type_: function_type,
                    body
                }))
            },
            &Expr::Application { ref function, ref arguments } => {
                let function_checked = self.check_expr(scope, function)?;
                let function_checked_type = function_checked.type_();
                let function_type;
                if let &Type::Function(ref type_) = &function_checked_type {
                    function_type = type_.clone()
                } else {
                    return error!("expected a function, found `{:?}' of type `{:?}'", function, function_checked_type);
                };

                let function_parameters = &function_type.parameters;
                let parameter_count = function_parameters.len();
                let argument_count = arguments.len();
                if argument_count != parameter_count {
                    return error!("invalid number of arguments: expected {}, found {}:\n  {:?}",
                        parameter_count, argument_count, expr);
                }

                let mut arguments_checked = vec![];
                for (parameter, argument) in function_parameters.iter().zip(arguments.iter()) {
                    let argument_checked = self.check_expr(scope, argument)?;
                    let argument_type = argument_checked.type_();
                    if &argument_type != &parameter.type_ {
                        return error!("argument type mismatch for {}: expected `{:?}', found `{:?}'\n  {:?}",
                            parameter.name, parameter.type_, argument_type, argument);
                    }
                    arguments_checked.push(argument_checked);
                }

                Ok(Rc::new(TypedExpr::Application {
                    type_: function_type.result.clone(),
                    function: function_checked,
                    arguments: arguments_checked
                }))
            },
        }
    }
}

type ScopeRef = Rc<Scope>;

struct Scope {
    bindings: RefCell<HashMap<String, (BindingRef, ExprRef)>>,
    outer_scope: Option<ScopeRef>
}

impl Scope {
    fn new(outer_scope: Option<ScopeRef>) -> ScopeRef {
        Rc::new(Scope {
            bindings: RefCell::new(HashMap::new()),
            outer_scope
        })
    }

    fn bind(&self, name: &str, binding: &BindingRef) -> Result<(), Box<Error>> {
        if self.bindings.borrow().contains_key(name) {
            error!("redefinition of {}", name)
        } else {
            let deref = Rc::new(TypedExpr::Deref(binding.clone()));
            self.bindings.borrow_mut().insert(name.to_string(), (binding.clone(), deref));
            Ok(())
        }
    }

    fn resolve(&self, name: &str) -> Result<ExprRef, Box<Error>> {
        self.bindings.borrow().get(name)
            .map(|&(_, ref deref)| deref.clone())
            .map_or_else(
                || self.outer_scope.clone().map_or_else(
                    || Err(From::from(format!("`{}' is undefined", name))),
                    |scope| scope.resolve(name)),
                Ok)
    }
}