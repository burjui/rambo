/*
The minimal SSA algorithm is based on the following paper:

Braun M., Buchwald S., Hack S., Leißa R., Mallon C., Zwinkau A.
(2013) Simple and Efficient Construction of Static Single Assignment Form.
In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
*/

use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use itertools::free::chain;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::identities::One;
use num_traits::identities::Zero;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::frontend::dead_code::remove_dead_code;
use crate::ir::BasicBlock;
use crate::ir::BasicBlockGraph;
use crate::ir::ControlFlowGraph;
use crate::ir::FnId;
use crate::ir::FunctionMap;
use crate::ir::get_value_operands;
use crate::ir::IdentGenerator;
use crate::ir::normalize;
use crate::ir::Phi;
use crate::ir::replace_value_id;
use crate::ir::Statement;
use crate::ir::StatementLocation;
use crate::ir::Value;
use crate::ir::value_storage::{UNDEFINED_VALUE, ValueId};
use crate::ir::value_storage::ValueStorage;
use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;

mod dead_code;
#[cfg(test)] mod tests;

pub(crate) struct FrontEnd {
    name: String,
    enable_warnings: bool,
    include_comments: bool,
    enable_cfp: bool,
    enable_dce: bool,
    graph: BasicBlockGraph,
    function_idgen: IdentGenerator<FnId>,
    variables: HashMap<BindingRef, HashMap<NodeIndex, ValueId>>,
    variables_read: HashSet<BindingRef>,
    values: ValueStorage,
    functions: FunctionMap,
    parameters: Vec<ValueId>,
    definitions: HashMap<ValueId, StatementLocation>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<BindingRef, ValueId>>,
    phi_users: HashMap<ValueId, HashSet<ValueId>>,
    entry_block: NodeIndex,
    undefined_users: Vec<ValueId>,
    markers: Vec<Option<Marker>>,
}

impl FrontEnd {
    pub(crate) fn new(name: &str) -> Self {
        let mut instance = Self {
            name: name.to_owned(),
            enable_warnings: false,
            include_comments: false,
            enable_cfp: false,
            enable_dce: false,
            graph: BasicBlockGraph::new(),
            function_idgen: IdentGenerator::new(),
            variables: HashMap::new(),
            variables_read: HashSet::new(),
            values: ValueStorage::new(),
            functions: HashMap::new(),
            parameters: Vec::new(),
            definitions: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_users: HashMap::new(),
            entry_block: NodeIndex::new(0),
            undefined_users: Vec::new(),
            markers: Vec::new(),
        };
        instance.entry_block = instance.new_block();
        instance.seal_block(instance.entry_block);
        instance
    }

    pub(crate) fn enable_warnings(mut self, value: bool) -> Self {
        self.enable_warnings = value;
        self
    }

    pub(crate) fn include_comments(mut self, value: bool) -> Self {
        self.include_comments = value;
        self
    }

    pub(crate) fn enable_cfp(mut self, value: bool) -> Self {
        self.enable_cfp = value;
        self
    }

    pub(crate) fn enable_dce(mut self, value: bool) -> Self {
        self.enable_dce = value;
        self
    }

    pub(crate) fn build(mut self, code: &ExprRef) -> ControlFlowGraph {
        let (exit_block, mut program_result) = self.process_expr(code, self.entry_block);
        self.push_return(exit_block, program_result);
        self.assert_no_undefined_variables();

        if self.enable_warnings {
            let variables = self.variables
                .into_iter()
                .map(|(variable, _)| variable)
                .collect::<HashSet<_>>();
            let redundant_bindings = variables.difference(&self.variables_read);
            warn_about_redundant_bindings(redundant_bindings);
        }

        if self.enable_dce {
            remove_dead_code(self.entry_block, &mut program_result, &mut self.values,
                             &mut self.graph, &mut self.definitions, &mut self.functions);
        }

        ControlFlowGraph {
            name: self.name,
            graph: self.graph,
            entry_block: self.entry_block,
            definitions: self.definitions,
            values: self.values,
            functions: self.functions,
            parameters: self.parameters,
            result: program_result,
        }
    }

    fn assert_no_undefined_variables(&self) {
        assert!(self.undefined_users.is_empty(), "found undefined usages:\n{:?}\n",
                self.undefined_users.iter()
                    .map(|user| &self.values[*user])
                    .format("\n"));
    }

    // TODO make block the first argument everywhere
    fn process_expr(&mut self, expr: &ExprRef, block: NodeIndex) -> (NodeIndex, ValueId) {
        match &**expr {
            TypedExpr::Block(statements, _) => {
                let mut current_block = block;
                let mut current_value_id = None;
                for statement in statements {
                    let (block, value_id) = self.process_statement(statement, current_block);
                    current_block = block;
                    current_value_id = Some(value_id);
                }
                let value_id = current_value_id.unwrap_or_else(|| self.define(block, Value::Unit));
                (current_block, value_id)
            }

            TypedExpr::Unit(source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Unit))
            }

            TypedExpr::Int(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Int(value.clone())))
            }

            TypedExpr::String(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::String(value.clone())))
            }

            TypedExpr::AddInt(left, right, source) => self.process_binary(block, left, right, source, Value::AddInt),
            TypedExpr::SubInt(left, right, source) => self.process_binary(block, left, right, source, Value::SubInt),
            TypedExpr::MulInt(left, right, source) => self.process_binary(block, left, right, source, Value::MulInt),
            TypedExpr::DivInt(left, right, source) => self.process_binary(block, left, right, source, Value::DivInt),
            TypedExpr::AddStr(left, right, source) => self.process_binary(block, left, right, source, Value::AddString),

            TypedExpr::Reference(binding, source) => {
                self.comment(block, source.text());
                (block, self.read_variable(binding, block))
            }

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                self.comment(block, source.text());
                let (block, condition) = self.process_expr(condition, block);
                let condition_value = &self.values[condition];

                if self.enable_cfp && condition_value.is_constant() {
                    return match condition_value {
                        Value::Int(n) => {
                            if n.is_zero() {
                                self.process_expr(negative, block)
                            } else {
                                self.process_expr(positive, block)
                            }
                        }

                        _ => unreachable!(),
                    };
                }

                let positive_block = self.new_block();
                let negative_block = self.new_block();

                self.graph[block].push(Statement::CondJump(condition, positive_block, negative_block));
                self.record_phi_and_undefined_usages(condition);

                self.graph.add_edge(block, positive_block, ());
                self.graph.add_edge(block, negative_block, ());
                self.seal_block(positive_block);
                self.seal_block(negative_block);
                let (positive_exit_block, positive) = self.process_expr(positive, positive_block);
                let (negative_exit_block, negative) = self.process_expr(negative, negative_block);

                let exit = self.new_block();
                self.graph.add_edge(positive_exit_block, exit, ());
                self.graph.add_edge(negative_exit_block, exit, ());
                self.seal_block(exit);
                let result_phi = self.define_phi(exit, &[positive, negative]);
                (exit, self.try_remove_trivial_phi(result_phi))
            }

            TypedExpr::Assign(binding, value, source) => {
                self.comment(block, source.text());
                let (block, value_id) = self.process_expr(value, block);
                self.write_variable(binding.clone(), block, value_id);
                (block, value_id)
            }

            TypedExpr::Lambda(lambda, source) => {
                let mut function = FrontEnd::new(source.text())
                    .enable_warnings(self.enable_warnings)
                    .include_comments(self.include_comments)
                    .enable_cfp(self.enable_cfp)
                    .enable_dce(self.enable_dce);
                for (index, parameter) in lambda.parameters.iter().enumerate() {
                    function.comment(function.entry_block, &parameter.name);
                    let declaration = function.define(function.entry_block, Value::Arg(index));
                    function.write_variable(parameter.clone(), function.entry_block, declaration);
                    function.parameters.push(declaration);
                }
                let fn_id = self.function_idgen.next_id();
                self.functions.insert(fn_id, function.build(&lambda.body));
                (block, self.define(block, Value::Function(fn_id)))
            }

            TypedExpr::Application { function, arguments, source, .. } => {
                self.comment(block, source.text());
                let (block, function_value_id) = self.process_expr(function, block);
                let mut current_block = block;
                let mut argument_value_ids = vec![];
                for argument in arguments {
                    let (block, value_id) = self.process_expr(argument, current_block);
                    argument_value_ids.push(value_id);
                    current_block = block;
                }
                (block, self.define(block, Value::Call(function_value_id, argument_value_ids)))
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, block: NodeIndex, left: &ExprRef, right: &ExprRef, source: &Source,
                      value_constructor: impl Fn(ValueId, ValueId) -> Value) -> (NodeIndex, ValueId)
    {
        let (block, left_value_id) = self.process_expr(left, block);
        let (block, right_value_id) = self.process_expr(right, block);
        self.comment(block, source.text());
        (block, self.define(block, value_constructor(left_value_id, right_value_id)))
    }

    fn process_statement(&mut self, statement: &TypedStatement, block: NodeIndex) -> (NodeIndex, ValueId) {
        match statement {
            TypedStatement::Expr(expr) => self.process_expr(expr, block),

            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, value_id) = self.process_expr(&binding.data, block);
                self.write_variable(binding.clone(), block, value_id);
                (block, value_id)
            },
        }
    }

    fn write_variable(&mut self, variable: BindingRef, block: NodeIndex, value_id: ValueId) {
        self.variables
            .entry(variable)
            .or_default()
            .insert(block, value_id);
    }

    fn read_variable(&mut self, variable: &BindingRef, block: NodeIndex) -> ValueId {
        self.reset_markers();
        self.read_variable_core(variable, block)
    }

    fn read_variable_core(&mut self, variable: &BindingRef, block: NodeIndex) -> ValueId {
        self.variables_read.insert(variable.clone());
        self.variables
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &BindingRef, block: NodeIndex) -> ValueId {
        let value_id = if !self.sealed_blocks.contains(&block) {
            // Incomplete CFG
            let phi = self.define_phi(block, &[]);
            self.incomplete_phis
                .entry(block)
                .or_default()
                .insert(variable.clone(), phi);
            phi
        } else {
            match self.get_marker(block) {
                None => self.mark_block(block, Marker::Initial),

                Some(Marker::Initial) => {
                    let phi = self.define_phi(block, &[]);
                    self.mark_block(block, Marker::Phi(phi));
                }

                _ => (),
            }

            let mut predecessors = self.graph
                .edges_directed(block, Direction::Incoming)
                .map(|edge| edge.source());
            let first_predecessor = predecessors.next();
            let second_predecessor = predecessors.next();
            if let (Some(predecessor), None) = (first_predecessor, second_predecessor) {
                // Optimize the common case of one predecessor: no phi needed
                self.read_variable_core(variable, predecessor)
            } else {
                // Break potential cycles with operandless phi
                let predecessors = chain(first_predecessor, second_predecessor)
                    .chain(predecessors)
                    .collect_vec();
                let definitions = predecessors
                    .into_iter()
                    .map(|predecessor| self.read_variable_core(variable, predecessor))
                    .dedup()
                    .collect_vec();
                self.remove_marker(block);
                match definitions.len() {
                    1 => definitions[0],
                    0 => UNDEFINED_VALUE,
                    _ => {
                        if definitions.contains(&UNDEFINED_VALUE) {
                            UNDEFINED_VALUE
                        } else {
                            let phi = match self.get_marker(block) {
                                Some(Marker::Phi(phi)) => *phi,
                                _ => self.define_phi(block, &[]),
                            };
                            let phi_operands = self.phi_operands_mut(phi);
                            *phi_operands = definitions;
                            phi_operands.sort();
                            phi
                        }
                    }
                }
            }
        };
        self.write_variable(variable.clone(), block, value_id);
        value_id
    }

    fn add_phi_operands(&mut self, variable: &BindingRef, phi: ValueId) -> ValueId {
        let predecessors = self.graph
            .edges_directed(self.definitions[&phi].block, Direction::Incoming)
            .map(|edge| edge.source())
            .collect_vec();
        for predecessor in predecessors {
            let operand = self.read_variable_core(variable, predecessor);
            self.phi_operands_mut(phi).push(operand);
        }
        self.phi_operands_mut(phi).sort();
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: ValueId) -> ValueId {
        let mut same = None;
        for &operand in self.phi_operands(phi) {
            if operand == UNDEFINED_VALUE {
                same = Some(operand);
                break;
            }
            if Some(operand) == same || operand == phi {
                // Unique value or self−reference
                continue;
            }
            if same.is_some() {
                // The phi merges at least two values: not trivial
                return phi;
            }
            same = Some(operand);
        }

        let same = match same {
            Some(value_id) => value_id,
            None => UNDEFINED_VALUE,
        };

        self.phi_users.get_mut(&phi).unwrap().remove(&phi);
        let mut possible_trivial_phis = HashSet::new();
        for user in &self.phi_users[&phi] {
            let value = &mut self.values[*user];
            replace_value_id(value, phi, same);
            if let Value::Phi(_) = value {
                possible_trivial_phis.insert(*user);
            }
        }

        for phi in possible_trivial_phis {
            self.try_remove_trivial_phi(phi);
        }

        same
    }

    fn seal_block(&mut self, block: NodeIndex) {
        let variables = self.incomplete_phis
            .get(&block)
            .map(|incomplete_phis| incomplete_phis
                .keys()
                .cloned()
                .collect_vec());
        if let Some(variables) = variables {
            for variable in &variables {
                self.add_phi_operands(variable, self.incomplete_phis[&block][variable]);
            }
        }
        self.sealed_blocks.insert(block);
    }

    fn define(&mut self, block: NodeIndex, value: Value) -> ValueId {
        let value = normalize(value);
        let (value_id, reused) = self.values.insert(value.clone());
        if !*reused {
            if self.enable_cfp {
                fold_constants(block, &self.definitions, &mut self.values, &self.functions, value_id);
            }
            let basic_block = &mut self.graph[block];
            let statement_index = basic_block.push(Statement::Definition(value_id));
            let location = StatementLocation::new(block, statement_index);
            self.definitions.insert(value_id, location);
            self.record_phi_and_undefined_usages(value_id);
            if let Value::Phi(_) = &self.values[value_id] {
                self.phi_users.insert(value_id, HashSet::new());
            }
        }
        value_id
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[ValueId]) -> ValueId {
        let mut operands = operands.to_vec();
        operands.sort();
        self.define(block, Value::Phi(Phi(operands)))
    }

    fn push_return(&mut self, block: NodeIndex, value_id: ValueId) {
        self.graph[block].push(Statement::Return(value_id));
    }

    fn record_phi_and_undefined_usages(&mut self, value_id: ValueId) {
        let (is_using_undefined, phis_used) = get_value_operands(&self.values[value_id])
            .fold((false, vec![]), |(mut is_using_undefined, mut phis_used), value_id| {
                if value_id == UNDEFINED_VALUE {
                    is_using_undefined = true;
                } else if self.is_a_phi(value_id) {
                    phis_used.push(value_id);
                }
                (is_using_undefined, phis_used)
            });
        if is_using_undefined {
            self.undefined_users.push(value_id);
        }
        for phi in phis_used {
            self.phi_users.get_mut(&phi).unwrap().insert(value_id);
        }
    }

    fn is_a_phi(&self, value_id: ValueId) -> bool {
        value_id != UNDEFINED_VALUE && self.phi_users.contains_key(&value_id)
    }

    fn phi_operands(&self, phi: ValueId) -> &[ValueId] {
        match &self.values[phi] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!()
        }
    }

    fn phi_operands_mut(&mut self, phi: ValueId) -> &mut Vec<ValueId> {
        match &mut self.values[phi] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!()
        }
    }

    fn comment(&mut self, block: NodeIndex, comment: &str) {
        if self.include_comments {
            self.graph[block].push(Statement::Comment(comment.to_owned()));
        }
    }

    fn new_block(&mut self) -> NodeIndex {
        self.graph.add_node(BasicBlock::new())
    }

    fn reset_markers(&mut self) {
        let len = self.markers.len();
        self.markers.truncate(0);
        self.markers.resize(len, None);
    }

    fn get_marker(&mut self, block: NodeIndex) -> &Option<Marker> {
        let index = self.marker_index(block);
        &self.markers[index]
    }

    fn mark_block(&mut self, block: NodeIndex, marker: Marker) {
        let index = self.marker_index(block);
        self.markers[index] = Some(marker);
    }

    fn remove_marker(&mut self, block: NodeIndex) {
        let index = self.marker_index(block);
        self.markers[index] = None;
    }

    fn marker_index(&mut self, block: NodeIndex) -> usize {
        let index = block.index();
        if index >= self.markers.len() {
            self.markers.resize(index +  1, None);
        }
        index
    }
}

#[derive(Copy, Clone)]
enum Marker {
    Initial,
    Phi(ValueId),
}

fn warn_about_redundant_bindings<'a>(redundant_bindings: impl Iterator<Item = &'a BindingRef>) {
    let redundant_bindings = redundant_bindings
        .sorted_by_key(|binding| binding.source.range().start());
    for binding in redundant_bindings {
        warning_at!(binding.source, "unused definition: {}", binding.source.text());
    }
}

fn fold_constants(
    block: NodeIndex,
    definitions: &HashMap<ValueId, StatementLocation>,
    values: &mut ValueStorage,
    functions: &FunctionMap,
    value_id: ValueId)
{
    let folded = match &values[value_id] {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function(_) => None,

        Value::AddInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left + right)),
                (Value::Int(left), _) if left.is_zero() => Some(right_value.clone()),
                (_, Value::Int(right)) if right.is_zero() => Some(left_value.clone()),
                _ => None,
            }
        }),

        Value::SubInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(left, left_value), (right, right_value)| {
            if left == right {
                Some(Value::Int(BigInt::from(0)))
            } else {
                match (left_value, right_value) {
                    (Value::Int(left), Value::Int(right)) => Some(Value::Int(left - right)),
                    (_, Value::Int(right)) if right.is_zero() => Some(left_value.clone()),
                    _ => None,
                }
            }
        }),

        Value::MulInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left * right)),
                (Value::Int(left), _) if left.is_zero() => Some(Value::Int(BigInt::from(0))),
                (_, Value::Int(right)) if right.is_zero() => Some(Value::Int(BigInt::from(0))),
                (Value::Int(left), _) if left.is_one() => Some(right_value.clone()),
                (_, Value::Int(right)) if right.is_one() => Some(left_value.clone()),
                _ => None,
            }
        }),

        Value::DivInt(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left / right)),
                (Value::Int(left), _) if left.is_zero() => Some(Value::Int(BigInt::from(0))),
                (_, Value::Int(right)) if right.is_one() => Some(left_value.clone()),
                _ => None,
            }
        }),

        Value::AddString(left, right) => fold_binary(block, definitions, values, functions, *left, *right, |(_, left_value), (_, right_value)| {
            match (left_value, right_value) {
                (Value::String(left), Value::String(right)) => {
                    let mut result = String::with_capacity(left.len() + right.len());
                    result.push_str(&left);
                    result.push_str(&right);
                    Some(Value::String(Rc::new(result)))
                },
                _ => None,
            }
        }),

        Value::Call(function, arguments) => fold_call(*function, arguments.to_vec(), values, functions),

        Value::Phi(_) |
        Value::Arg(_) => None,
    };
    if let Some(value) = folded {
        values[value_id] = value;
    }
}

fn fold_binary(
    block: NodeIndex,
    definitions: &HashMap<ValueId, StatementLocation>,
    values: &mut ValueStorage,
    functions: &FunctionMap,
    left: ValueId, right: ValueId,
    fold: impl FnOnce((ValueId, &Value), (ValueId, &Value)) -> Option<Value>) -> Option<Value>
{
    fold_constants(block, definitions, values, functions, left);
    fold_constants(block, definitions, values, functions, right);
    fold((left, &values[left]), (right, &values[right]))
}

fn fold_call(
    function: ValueId,
    arguments: Vec<ValueId>,
    values: &mut ValueStorage,
    functions: &FunctionMap) -> Option<Value>
{
    let arguments_are_constant = arguments
        .iter()
        .all(|argument| values[*argument].is_constant());
    if arguments_are_constant {
        let mut function_cfg = match &values[function] {
            Value::Function(fn_id) => functions[fn_id].clone(),
            _ => unreachable!(),
        };
        for (parameter_value_id, argument_value_id) in function_cfg.parameters.iter().enumerate() {
            function_cfg.values[*argument_value_id] =
                values[arguments[parameter_value_id]].clone();
        }

        let result_block = function_cfg.definitions[&function_cfg.result].block;
        fold_constants(result_block, &function_cfg.definitions,
                       &mut function_cfg.values, functions, function_cfg.result);
        let result_value = &function_cfg.values[function_cfg.result];
        if result_value.is_constant() {
            return Some(result_value.clone())
        }
    }
    None
}
