use crate::semantics::BindingRef;
use crate::semantics::BindingValue;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::FunctionTypeRef;
use crate::semantics::Lambda;
use crate::semantics::Type;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use num_bigint::BigInt;
use std::rc::Rc;

crate trait TypedVisitor {
    fn visit_statements(&mut self, statements: &[TypedStatement]) -> Vec<TypedStatement> {
        let statements = statements.iter()
            .filter_map(|statement| self.visit_statement(statement))
            .collect::<Vec<_>>();
        self.post_statements(statements)
    }

    fn post_statements(&mut self, statements: Vec<TypedStatement>) -> Vec<TypedStatement> {
        statements.to_vec()
    }

    fn visit_statement(&mut self, statement: &TypedStatement) -> Option<TypedStatement> {
        match statement {
            TypedStatement::Binding(binding) => Some(TypedStatement::Binding(self.visit_binding(binding))),
            TypedStatement::Expr(expr) => Some(TypedStatement::Expr(self.visit(expr)))
        }
    }

    fn visit(&mut self, expr: &ExprRef) -> ExprRef {
        match expr as &TypedExpr {
            TypedExpr::Phantom => self.visit_phantom(expr),
            TypedExpr::Unit(source) => self.visit_unit(expr, source),
            TypedExpr::Int(value, source) => self.visit_int(expr, value, source),
            TypedExpr::String(value, source) => self.visit_string(expr, value, source),
            TypedExpr::Deref(binding, source) => self.visit_deref(expr, binding, source),
            TypedExpr::Lambda(lambda, source) => self.visit_lambda(expr, lambda, source),
            TypedExpr::Application { type_, function, arguments, source } =>
                self.visit_application(expr, type_, function, arguments, source),
            TypedExpr::AddInt(left, right, source) => self.visit_addint(expr, left, right, source),
            TypedExpr::SubInt(left, right, source) => self.visit_subint(expr, left, right, source),
            TypedExpr::MulInt(left, right, source) => self.visit_mulint(expr, left, right, source),
            TypedExpr::DivInt(left, right, source) => self.visit_divint(expr, left, right, source),
            TypedExpr::AddStr(left, right, source) => self.visit_addstr(expr, left, right, source),
            TypedExpr::Assign(binding, value, source) => self.visit_assign(expr, binding, value, source),
            TypedExpr::Conditional { condition, positive, negative, source } =>
                self.visit_conditional(expr, condition, positive, negative, source),
            TypedExpr::Block(block) => ExprRef::from(TypedExpr::Block(self.visit_block(&block))),
        }
    }

    fn visit_phantom(&mut self, expr: &ExprRef) -> ExprRef {
        expr.clone()
    }

    #[allow(unused)]
    fn visit_unit(&mut self, expr: &ExprRef, source: &Source) -> ExprRef {
        expr.clone()
    }

    #[allow(unused)]
    fn visit_int(&mut self, expr: &ExprRef, value: &BigInt, source: &Source) -> ExprRef {
        expr.clone()
    }

    #[allow(unused)]
    fn visit_string(&mut self, expr: &ExprRef, value: &str, source: &Source) -> ExprRef {
        expr.clone()
    }

    fn visit_deref(&mut self, expr: &ExprRef, binding: &BindingRef, source: &Source) -> ExprRef {
        let binding = self.visit_binding(binding);
        self.post_deref(expr, binding, source)
    }

    #[allow(unused)]
    fn post_deref(&mut self, expr: &ExprRef, binding: BindingRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::Deref(binding, source.clone()))
    }

    fn visit_binding(&mut self, binding: &BindingRef) -> BindingRef {
        let mut binding_mut = binding.borrow_mut();
        if let BindingValue::Var(value) = &binding_mut.data {
            binding_mut.data = BindingValue::Var(self.visit(value))
        }
        binding.clone()
    }

    fn visit_lambda(&mut self, expr: &ExprRef, lambda: &Rc<Lambda>, source: &Source) -> ExprRef {
        let parameters: Vec<BindingRef> = lambda.parameters.iter().
            map(|parameter| self.visit_binding(parameter)).collect();
        let body = self.visit(&lambda.body);
        self.post_lambda(expr, &lambda.type_, parameters, body, source)
    }

    #[allow(unused)]
    fn post_lambda(&mut self, expr: &ExprRef, type_: &FunctionTypeRef,
        parameters: Vec<BindingRef>, body: ExprRef, source: &Source) -> ExprRef
    {
        let lambda = Rc::new(Lambda { type_: type_.clone(), parameters, body });
        ExprRef::from(TypedExpr::Lambda(lambda, source.clone()))
    }

    fn visit_application(&mut self, expr: &ExprRef, type_: &Type,
        function: &ExprRef, arguments: &[ExprRef], source: &Source) -> ExprRef
    {
        let function = self.visit(function);
        let arguments: Vec<ExprRef> = arguments.iter().map(|argument| self.visit(argument)).collect();
        self.post_application(expr, type_, function, arguments, source)
    }

    #[allow(unused)]
    fn post_application(&mut self, expr: &ExprRef, type_: &Type,
        function: ExprRef, arguments: Vec<ExprRef>, source: &Source) -> ExprRef
    {
        ExprRef::from(TypedExpr::Application {
            type_: type_.clone(),
            function,
            arguments,
            source: source.clone()
        })
    }

    fn visit_addint(&mut self, expr: &ExprRef, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef {
        let left = self.visit(left);
        let right = self.visit(right);
        self.post_addint(expr, left, right, source)
    }

    #[allow(unused)]
    fn post_addint(&mut self, expr: &ExprRef, left: ExprRef, right: ExprRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::AddInt(left.clone(), right.clone(), source.clone()))
    }

    fn visit_subint(&mut self, expr: &ExprRef, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef {
        let left = self.visit(left);
        let right = self.visit(right);
        self.post_subint(expr, left, right, source)
    }

    #[allow(unused)]
    fn post_subint(&mut self, expr: &ExprRef, left: ExprRef, right: ExprRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::SubInt(left.clone(), right.clone(), source.clone()))
    }

    fn visit_mulint(&mut self, expr: &ExprRef, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef {
        let left = self.visit(left);
        let right = self.visit(right);
        self.post_mulint(expr, left, right, source)
    }

    #[allow(unused)]
    fn post_mulint(&mut self, expr: &ExprRef, left: ExprRef, right: ExprRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::MulInt(left.clone(), right.clone(), source.clone()))
    }

    fn visit_divint(&mut self, expr: &ExprRef, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef {
        let left = self.visit(left);
        let right = self.visit(right);
        self.post_divint(expr, left, right, source)
    }

    #[allow(unused)]
    fn post_divint(&mut self, expr: &ExprRef, left: ExprRef, right: ExprRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::DivInt(left.clone(), right.clone(), source.clone()))
    }

    fn visit_addstr(&mut self, expr: &ExprRef, left: &ExprRef, right: &ExprRef, source: &Source) -> ExprRef {
        let left = self.visit(left);
        let right = self.visit(right);
        self.post_addstr(expr, left, right, source)
    }

    #[allow(unused)]
    fn post_addstr(&mut self, expr: &ExprRef, left: ExprRef, right: ExprRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::AddStr(left.clone(), right.clone(), source.clone()))
    }

    fn visit_assign(&mut self, expr: &ExprRef, binding: &BindingRef, value: &ExprRef, source: &Source) -> ExprRef {
        let binding = self.visit_binding(binding);
        let value = self.visit(value);
        self.post_assign(expr, binding, value, source)
    }

    #[allow(unused)]
    fn post_assign(&mut self, expr: &ExprRef, binding: BindingRef, value: ExprRef, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::Assign(binding, value, source.clone()))
    }

    fn visit_conditional(&mut self, expr: &ExprRef, condition: &ExprRef, positive: &ExprRef, negative: &Option<ExprRef>, source: &Source) -> ExprRef {
        let condition = self.visit(condition);
        let positive = self.visit(positive);
        let negative = negative.as_ref().map(|clause| self.visit(clause));
        self.post_conditional(expr, condition, positive, negative, source)
    }

    #[allow(unused)]
    fn post_conditional(&mut self, expr: &ExprRef, condition: ExprRef, positive: ExprRef, negative: Option<ExprRef>, source: &Source) -> ExprRef {
        ExprRef::from(TypedExpr::Conditional {
            condition, positive, negative, source: source.clone()
        })
    }

    fn visit_block(&mut self, block: &Block) -> Block {
        let statements = self.visit_statements(&block.statements);
        self.post_block(statements, &block.source)
    }

    #[allow(unused)]
    fn post_block(&mut self, statements: Vec<TypedStatement>, source: &Source) -> Block {
        Block {
            statements,
            source: source.clone()
        }
    }
}
