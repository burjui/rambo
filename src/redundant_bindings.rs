use crate::env::Environment;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::Lambda;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

crate struct Warnings(crate bool);

crate fn report_redundant_bindings(code: &ExprRef, Warnings(warnings): Warnings) {
    let mut detector = Detector {
        env: Environment::new(),
        binding_usages: HashMap::new(),
        lambdas_processed: HashSet::new(),
    };
    detector.process(code);
    if warnings {
        let mut redundant_bindings = detector.binding_usages.iter()
            .filter_map(|(source, usages)| if *usages == 0 { Some(source.clone()) } else { None })
            .collect::<Vec<_>>();
        redundant_bindings.sort_unstable_by(|s1, s2| s1.range.start.cmp(&s2.range.start));
        for source in redundant_bindings {
            warning_at!(source, "unused definition: {}", source.text())
        }
    }
}

struct Detector {
    env: Environment<Rc<String>, Source>,
    binding_usages: HashMap<Source, usize>,
    lambdas_processed: HashSet<LambdaRef>,
}

#[derive(Clone)]
struct LambdaRef(Rc<Lambda>);

impl PartialEq<LambdaRef> for LambdaRef {
    fn eq(&self, other: &LambdaRef) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for LambdaRef {}

impl Hash for LambdaRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::into_raw(self.0.clone()).hash(state)
    }
}

impl Detector {
    fn process(&mut self, expr: &ExprRef) {
        match expr as &TypedExpr {
            TypedExpr::Deref(name, _, _) => {
                let binding = self.env.resolve(name).unwrap();
                (*self.binding_usages.get_mut(&binding).unwrap()) += 1;
            },
            TypedExpr::AddInt(left, right, _) |
            TypedExpr::SubInt(left, right, _) |
            TypedExpr::MulInt(left, right, _) |
            TypedExpr::DivInt(left, right, _) |
            TypedExpr::AddStr(left, right, _) => {
                self.process(left);
                self.process(right);
            },
            TypedExpr::Assign(name, value, _) => {
                let binding = self.env.resolve(name).unwrap();
                (*self.binding_usages.get_mut(&binding).unwrap()) += 1;
                self.process(value);
            },
            TypedExpr::Application { function, arguments, .. } => {
                self.process(function);
                for argument in arguments {
                    self.process(argument);
                }
            },
            TypedExpr::Lambda(lambda, _) => {
                let cache_key = LambdaRef(lambda.clone());
                if !self.lambdas_processed.contains(&cache_key) {
                    self.lambdas_processed.insert(cache_key);
                    self.env.push();
                    for parameter in &lambda.parameters {
                        let name = Rc::new(parameter.name.text().to_owned());
                        self.register_binding(name, parameter.name.clone());
                    }
                    self.process(&lambda.body);
                    self.env.pop();
                }
            }
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                self.process(condition);
                self.process(positive);
                if let Some(negative) = negative {
                    self.process(negative);
                }
            },
            TypedExpr::Block(block) => self.process_block(block),
            TypedExpr::ArgumentPlaceholder(_) |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => {}
        }
    }

    fn process_block(&mut self, block: &Block) {
        self.env.push();
        for statement in &block.statements {
            self.process_statement(statement);
        }
        self.env.pop();
    }

    fn process_statement(&mut self, statement: &TypedStatement) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.process(&binding.data);
                self.register_binding(binding.name.clone(), binding.source.clone());
            },
            TypedStatement::Expr(expr) => self.process(expr)
        }
    }

    fn register_binding(&mut self, name: Rc<String>, source: Source) {
        self.env.bind(name, source.clone());
        assert!(self.binding_usages.insert(source, 0).is_none());
    }
}
