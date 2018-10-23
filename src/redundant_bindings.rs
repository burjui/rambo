use crate::env::Environment;
use crate::semantics::Binding;
use crate::semantics::BindingRef;
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
        redundant_bindings: Vec::new()
    };
    detector.process(code);
    if warnings {
        detector.redundant_bindings.sort_by(|s1, s2| s1.range.start.cmp(&s2.range.start));
        for source in detector.redundant_bindings {
            warning_at!(source, "unused definition: {}", source.text())
        }
    }
}

struct Detector {
    env: Environment<String, BindingRef>,
    binding_usages: HashMap<BindingRef, usize>,
    lambdas_processed: HashSet<LambdaRef>,
    redundant_bindings: Vec<Source>,
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
                self.register_binding_usages(binding.clone());
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
                        let name = parameter.name.text().to_owned();
                        let binding = Self::new_fake_binding(name.clone(), parameter.name.clone());
                        self.env.bind(name, binding.clone());
                        self.register_binding_usages(binding);
                    }
                    self.process(&lambda.body);
                    self.pop_env();
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
            TypedExpr::Phantom(_) |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) |
            TypedExpr::Reference(_, _) => {}
        }
    }

    fn process_block(&mut self, block: &Block) {
        let mut bindings = Vec::<BindingRef>::new();
        self.env.push();
        for statement in &block.statements {
            self.process_statement(statement);
            if let TypedStatement::Binding(binding) = statement {
                bindings.push(binding.clone());
            }
        }
        self.pop_env();
    }

    fn process_statement(&mut self, statement: &TypedStatement) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.process(&binding.borrow().data);
                self.env.bind(binding.borrow().name.clone(), binding.clone());
                self.register_binding_usages(binding.clone());
            },
            TypedStatement::Expr(expr) => self.process(expr)
        }
    }

    fn register_binding_usages(&mut self, binding: BindingRef) {
        self.binding_usages.entry(binding).or_insert(0);
    }

    fn pop_env(&mut self) {
        let scope_log = self.env.pop();
        for (_, binding) in scope_log {
            if self.binding_usages[&binding] == 0 {
                self.redundant_bindings.push(binding.borrow().source.clone());
            }
        }
    }

    fn new_fake_binding(name: String, source: Source) -> BindingRef {
        BindingRef::from(Binding {
            name,
            data: ExprRef::from(TypedExpr::Unit(source.clone())),
            source,
            assigned: false,
            dirty: false,
        })
    }
}
