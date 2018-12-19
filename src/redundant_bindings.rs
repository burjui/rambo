use hashbrown::HashMap;
use hashbrown::HashSet;

use crate::semantics::BindingKind;
use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::LambdaRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;

// TODO rewrite for SSA

crate struct Warnings(crate bool);

crate fn report_redundant_bindings(code: &ExprRef, Warnings(warnings): Warnings) {
    let mut detector = Detector {
        binding_usages: HashMap::new(),
        lambdas_processed: HashSet::new(),
    };
    detector.process(code);
    if warnings {
        let mut redundant_bindings = detector.binding_usages.iter()
            .filter_map(|(binding, usages)| if *usages == 0 { Some(binding.clone()) } else { None })
            .collect::<Vec<_>>();
        redundant_bindings.sort_unstable_by(|s1, s2| s1.source.range().start().cmp(&s2.source.range().start()));
        for binding in redundant_bindings {
            let kind = match binding.kind {
                BindingKind::Let => "definition",
                BindingKind::Arg(_) => "argument"
            };
            warning_at!(&binding.source, "unused {}: {}", kind, binding.source.text())
        }
    }
}

struct Detector {
    binding_usages: HashMap<BindingRef, usize>,
    lambdas_processed: HashSet<LambdaRef>,
}

impl Detector {
    fn process(&mut self, expr: &ExprRef) {
        match expr as &TypedExpr {
            TypedExpr::Reference(binding, _) => *self.usages(binding) += 1,
            TypedExpr::AddInt(left, right, _) |
            TypedExpr::SubInt(left, right, _) |
            TypedExpr::MulInt(left, right, _) |
            TypedExpr::DivInt(left, right, _) |
            TypedExpr::AddStr(left, right, _) => {
                self.process(left);
                self.process(right);
            },
            TypedExpr::Assign(binding, value, _) => {
                *self.usages(binding) += 1;
                self.process(value);
            },
            TypedExpr::Application { function, arguments, .. } => {
                self.process(function);
                for argument in arguments {
                    self.process(argument);
                }
            },
            TypedExpr::Lambda(lambda, _) => {
                let cache_key = lambda.clone();
                if !self.lambdas_processed.contains(&cache_key) {
                    self.lambdas_processed.insert(cache_key);
                    for parameter in &lambda.parameters {
                        self.usages(parameter);
                    }
                    self.process(&lambda.body);
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
            TypedExpr::ArgumentPlaceholder(_, _) |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => {}
        }
    }

    fn process_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.process_statement(statement);
        }
    }

    fn process_statement(&mut self, statement: &TypedStatement) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.usages(binding);
                self.process(&binding.data);
            },
            TypedStatement::Expr(expr) => self.process(expr)
        }
    }

    fn usages(&mut self, binding: &BindingRef) -> &mut usize {
        self.binding_usages.entry(binding.clone()).or_insert(0)
    }
}
