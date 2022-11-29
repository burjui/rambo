/*
The minimal SSA algorithm is based on the following paper:

Braun M., Buchwald S., Hack S., Leißa R., Mallon C., Zwinkau A.
(2013) Simple and Efficient Construction of Static Single Assignment Form.
In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
*/

use std::rc::Rc;

use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    frontend::dead_code::remove_dead_code,
    ir::{
        get_value_operands,
        replace_value_id,
        value_storage::{ValueId, ValueStorage, UNDEFINED_VALUE},
        ControlFlowGraph,
        FnId,
        FnIdGenerator,
        FunctionMap,
        IRModule,
        Phi,
        Statement,
        StatementLocation,
        Value,
    },
    semantics::{BindingRef, ExprRef, TypedExpr, TypedStatement},
    source::Source,
    stable_graph::{Direction, NodeIndex},
};

mod dead_code;
#[cfg(test)]
mod tests;

pub(crate) struct FrontEndState {
    function_idgen: FnIdGenerator,
    main_fn_id: Option<FnId>,
}

impl FrontEndState {
    pub(crate) const fn new() -> Self {
        Self {
            function_idgen: FnIdGenerator::new(),
            main_fn_id: None,
        }
    }
}

pub(crate) struct FrontEnd<'a> {
    name: String,
    include_comments: bool,
    enable_cfp: bool,
    enable_dce: bool,
    cfg: ControlFlowGraph,
    state: &'a mut FrontEndState,
    functions: FunctionMap,
    variables: FxHashMap<BindingRef, FxHashMap<NodeIndex, ValueId>>,
    values: ValueStorage,
    parameters: Vec<ValueId>,
    definitions: FxHashMap<ValueId, StatementLocation>,
    sealed_blocks: FxHashSet<NodeIndex>,
    incomplete_phis: FxHashMap<NodeIndex, FxHashMap<BindingRef, ValueId>>,
    phi_users: FxHashMap<ValueId, FxHashSet<ValueId>>,
    entry_block: NodeIndex,
    undefined_users: Vec<ValueId>,
    markers: FxHashMap<NodeIndex, Marker>,
}

impl<'a> FrontEnd<'a> {
    pub(crate) fn new(name: &str, state: &'a mut FrontEndState) -> Self {
        let mut instance = Self {
            name: name.to_owned(),
            include_comments: false,
            enable_cfp: false,
            enable_dce: false,
            cfg: ControlFlowGraph::new(),
            state,
            functions: FxHashMap::default(),
            variables: FxHashMap::default(),
            values: ValueStorage::new(),
            parameters: Vec::new(),
            definitions: FxHashMap::default(),
            sealed_blocks: FxHashSet::default(),
            incomplete_phis: FxHashMap::default(),
            phi_users: FxHashMap::default(),
            entry_block: NodeIndex::default(),
            undefined_users: Vec::new(),
            markers: FxHashMap::default(),
        };
        instance.entry_block = instance.cfg.add_new_block();
        instance.seal_block(instance.entry_block);
        instance
    }

    pub(crate) const fn include_comments(mut self, value: bool) -> Self {
        self.include_comments = value;
        self
    }

    pub(crate) const fn enable_cfp(mut self, value: bool) -> Self {
        self.enable_cfp = value;
        self
    }

    pub(crate) const fn enable_dce(mut self, value: bool) -> Self {
        self.enable_dce = value;
        self
    }

    pub(crate) fn build(mut self, code: &ExprRef) -> IRModule {
        let (exit_block, mut program_result) = self.process_expr(code, self.entry_block);
        self.push_return(exit_block, program_result);
        self.assert_no_undefined_variables();

        if self.enable_dce {
            remove_dead_code(
                self.entry_block,
                &mut program_result,
                &mut self.values,
                &mut self.cfg,
                &mut self.definitions,
                &mut self.functions,
            );
        }

        IRModule {
            name: self.name,
            cfg: self.cfg,
            entry_block: self.entry_block,
            values: self.values,
            functions: self.functions,
            parameters: self.parameters,
            result: program_result,
        }
    }

    fn assert_no_undefined_variables(&self) {
        assert!(
            self.undefined_users.is_empty(),
            "found undefined usages:\n{:?}\n",
            self.undefined_users
                .iter()
                .map(|user| &self.values[*user])
                .format("\n")
        );
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
                (block, self.define(block, Value::Int(*value)))
            }

            TypedExpr::String(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::String(value.clone())))
            }

            TypedExpr::AddInt(left, right, source) => {
                self.process_binary(block, left, right, source, Value::AddInt)
            }
            TypedExpr::SubInt(left, right, source) => {
                self.process_binary(block, left, right, source, Value::SubInt)
            }
            TypedExpr::MulInt(left, right, source) => {
                self.process_binary(block, left, right, source, Value::MulInt)
            }
            TypedExpr::DivInt(left, right, source) => {
                self.process_binary(block, left, right, source, Value::DivInt)
            }
            TypedExpr::AddStr(left, right, source) => {
                self.process_binary(block, left, right, source, Value::AddString)
            }

            TypedExpr::Reference(binding, source) => {
                self.comment(block, source.text());
                (block, self.read_variable(binding, block))
            }

            TypedExpr::Conditional {
                condition,
                then_branch,
                else_branch,
                source,
                ..
            } => {
                self.comment(block, source.text());
                let (block, condition) = self.process_expr(condition, block);

                // FIXME this fails to report unused bindings in other branch, probably should make CFP a separate pass
                //       OR check unused bindings in semantics pass
                if self.enable_cfp {
                    if let Value::Int(value) = &self.values[condition] {
                        return if *value == 0 {
                            self.process_expr(else_branch, block)
                        } else {
                            self.process_expr(then_branch, block)
                        };
                    }
                }

                let then_branch_block = self.cfg.add_new_block();
                let else_branch_block = self.cfg.add_new_block();

                self.cfg[block].push(Statement::CondJump(
                    condition,
                    then_branch_block,
                    else_branch_block,
                ));
                self.record_phi_and_undefined_usages(condition);

                self.cfg.add_edge(block, then_branch_block);
                self.cfg.add_edge(block, else_branch_block);
                self.seal_block(then_branch_block);
                self.seal_block(else_branch_block);
                let (then_branch_exit_block, then_branch) =
                    self.process_expr(then_branch, then_branch_block);
                let (else_branch_exit_block, else_branch) =
                    self.process_expr(else_branch, else_branch_block);

                let exit = self.cfg.add_new_block();
                self.cfg.add_edge(then_branch_exit_block, exit);
                self.cfg.add_edge(else_branch_exit_block, exit);
                self.seal_block(exit);
                let phi = self.define_phi(exit, &[then_branch, else_branch]);
                (exit, self.try_remove_trivial_phi(phi))
            }

            TypedExpr::Assign(binding, value, source) => {
                self.comment(block, source.text());
                let (block, value_id) = self.process_expr(value, block);
                self.write_variable(binding.clone(), block, value_id);
                (block, value_id)
            }

            TypedExpr::Function(function, source) => {
                let fn_id = self.state.function_idgen.new_id(source.clone());
                if function.name.as_str() == "main" {
                    self.state.main_fn_id = Some(fn_id.clone());
                }

                let mut function_frontend = FrontEnd::new(function.name.as_str(), self.state)
                    .include_comments(self.include_comments)
                    .enable_cfp(self.enable_cfp)
                    .enable_dce(self.enable_dce);
                for (index, parameter) in function.parameters.iter().enumerate() {
                    function_frontend.comment(function_frontend.entry_block, &parameter.name);
                    let declaration =
                        function_frontend.define(function_frontend.entry_block, Value::Arg(index));
                    function_frontend.write_variable(
                        parameter.clone(),
                        function_frontend.entry_block,
                        declaration,
                    );
                    function_frontend.parameters.push(declaration);
                }
                let module = function_frontend.build(&function.body);
                self.functions.insert(fn_id.clone(), module);
                (
                    block,
                    self.define(block, Value::Function(fn_id, function.name.clone())),
                )
            }

            TypedExpr::Application {
                function,
                arguments,
                source,
                ..
            } => {
                self.comment(block, source.text());
                let (block, function_value_id) = self.process_expr(function, block);
                let mut current_block = block;
                let mut argument_value_ids = vec![];
                for argument in arguments {
                    let (block, value_id) = self.process_expr(argument, current_block);
                    argument_value_ids.push(value_id);
                    current_block = block;
                }
                (
                    block,
                    self.define(block, Value::Call(function_value_id, argument_value_ids)),
                )
            }

            _ => unimplemented!("process_expr: {:?}", expr),
        }
    }

    fn process_binary(
        &mut self,
        block: NodeIndex,
        left: &ExprRef,
        right: &ExprRef,
        source: &Source,
        value_constructor: impl Fn(ValueId, ValueId) -> Value,
    ) -> (NodeIndex, ValueId) {
        let (block, left_value_id) = self.process_expr(left, block);
        let (block, right_value_id) = self.process_expr(right, block);
        self.comment(block, source.text());
        (
            block,
            self.define(block, value_constructor(left_value_id, right_value_id)),
        )
    }

    fn process_statement(
        &mut self,
        statement: &TypedStatement,
        block: NodeIndex,
    ) -> (NodeIndex, ValueId) {
        match statement {
            TypedStatement::Expr(expr) => self.process_expr(expr, block),

            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, value_id) = self.process_expr(&binding.data, block);
                self.write_variable(binding.clone(), block, value_id);
                (block, value_id)
            }
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
        self.variables
            .get(variable)
            .and_then(|map| map.get(&block).cloned())
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &BindingRef, block: NodeIndex) -> ValueId {
        let value_id = if self.sealed_blocks.contains(&block) {
            match self.get_marker(block) {
                None => self.mark_block(block, Marker::Initial),

                Some(Marker::Initial) => {
                    let phi = self.define_phi(block, &[]);
                    self.mark_block(block, Marker::Phi(phi));
                }

                _ => (),
            }

            let predecessors = self
                .cfg
                .edges_directed(block, Direction::Incoming)
                .map(|edge| edge.source)
                .collect_vec();
            let first_predecessor = predecessors.get(0).cloned();
            let second_predecessor = predecessors.get(1).cloned();
            if let (Some(predecessor), None) = (first_predecessor, second_predecessor) {
                // Optimize the common case of one predecessor: no phi needed
                self.read_variable_core(variable, predecessor)
            } else {
                // Break potential cycles with operandless phi
                let definitions = predecessors
                    .iter()
                    .map(|predecessor| self.read_variable_core(variable, *predecessor))
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
        } else {
            // Incomplete CFG
            let phi = self.define_phi(block, &[]);
            self.incomplete_phis
                .entry(block)
                .or_default()
                .insert(variable.clone(), phi);
            phi
        };
        self.write_variable(variable.clone(), block, value_id);
        value_id
    }

    fn add_phi_operands(&mut self, variable: &BindingRef, phi: ValueId) -> ValueId {
        let predecessors = self
            .cfg
            .edges_directed(self.definitions[&phi].block, Direction::Incoming)
            .map(|edge| edge.source)
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
        let mut possible_trivial_phis = FxHashSet::default();
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
        let variables = self
            .incomplete_phis
            .get(&block)
            .map(|incomplete_phis| incomplete_phis.keys().cloned().collect_vec());
        if let Some(variables) = variables {
            for variable in &variables {
                self.add_phi_operands(variable, self.incomplete_phis[&block][variable]);
            }
        }
        self.sealed_blocks.insert(block);
    }

    fn define(&mut self, block: NodeIndex, value: Value) -> ValueId {
        let value_id = self.values.insert(value);
        if self.enable_cfp {
            fold_constants(&mut self.values, &self.functions, value_id);
        }
        let basic_block = &mut self.cfg[block];
        let statement_index = basic_block.push(Statement::Definition(value_id));
        let location = StatementLocation::new(block, statement_index);
        self.definitions.insert(value_id, location);
        self.record_phi_and_undefined_usages(value_id);
        if let Value::Phi(_) = &self.values[value_id] {
            self.phi_users.insert(value_id, FxHashSet::default());
        }
        value_id
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[ValueId]) -> ValueId {
        let mut operands = operands.to_vec();
        operands.sort();
        self.define(block, Value::Phi(Phi(operands)))
    }

    fn push_return(&mut self, block: NodeIndex, value_id: ValueId) {
        self.cfg[block].push(Statement::Return(value_id));
    }

    fn record_phi_and_undefined_usages(&mut self, value_id: ValueId) {
        let (is_using_undefined, phis_used) = get_value_operands(&self.values[value_id]).fold(
            (false, vec![]),
            |(mut is_using_undefined, mut phis_used), value_id| {
                if value_id == UNDEFINED_VALUE {
                    is_using_undefined = true;
                } else if self.is_a_phi(value_id) {
                    phis_used.push(value_id);
                }
                (is_using_undefined, phis_used)
            },
        );
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
            _ => unreachable!(),
        }
    }

    fn phi_operands_mut(&mut self, phi: ValueId) -> &mut Vec<ValueId> {
        match &mut self.values[phi] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!(),
        }
    }

    fn comment(&mut self, block: NodeIndex, comment: &str) {
        if self.include_comments {
            self.cfg[block].push(Statement::Comment(comment.to_owned()));
        }
    }

    fn reset_markers(&mut self) {
        self.markers.clear();
    }

    fn get_marker(&mut self, block: NodeIndex) -> Option<&Marker> {
        self.markers.get(&block)
    }

    fn mark_block(&mut self, block: NodeIndex, marker: Marker) {
        self.markers.insert(block, marker);
    }

    fn remove_marker(&mut self, block: NodeIndex) {
        self.markers.remove(&block);
    }
}

#[derive(Copy, Clone)]
enum Marker {
    Initial,
    Phi(ValueId),
}

fn fold_constants(values: &mut ValueStorage, functions: &FunctionMap, value_id: ValueId) {
    let folded = match &values[value_id] {
        &Value::AddInt(left, right) => fold_binary(
            values,
            functions,
            left,
            right,
            |(_, left_value), (_, right_value)| match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left + right)),
                (&Value::Int(left), _) if left == 0 => Some(right_value.clone()),
                (_, &Value::Int(right)) if right == 0 => Some(left_value.clone()),
                _ => None,
            },
        ),

        &Value::SubInt(left, right) => fold_binary(
            values,
            functions,
            left,
            right,
            |(left, left_value), (right, right_value)| {
                if left == right {
                    Some(Value::Int(0))
                } else {
                    match (left_value, right_value) {
                        (Value::Int(left), Value::Int(right)) => Some(Value::Int(left - right)),
                        (_, &Value::Int(right)) if right == 0 => Some(left_value.clone()),
                        _ => None,
                    }
                }
            },
        ),

        &Value::MulInt(left, right) => fold_binary(
            values,
            functions,
            left,
            right,
            |(_, left_value), (_, right_value)| match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left * right)),
                (&Value::Int(left), _) if left == 0 => Some(Value::Int(0)),
                (_, &Value::Int(right)) if right == 0 => Some(Value::Int(0)),
                (&Value::Int(left), _) if left == 1 => Some(right_value.clone()),
                (_, &Value::Int(right)) if right == 1 => Some(left_value.clone()),
                _ => None,
            },
        ),

        &Value::DivInt(left, right) => fold_binary(
            values,
            functions,
            left,
            right,
            |(_, left_value), (_, right_value)| match (left_value, right_value) {
                (Value::Int(left), Value::Int(right)) => Some(Value::Int(left / right)),
                (&Value::Int(left), _) if left == 0 => Some(Value::Int(0)),
                (_, &Value::Int(right)) if right == 1 => Some(left_value.clone()),
                _ => None,
            },
        ),

        &Value::AddString(left, right) => fold_binary(
            values,
            functions,
            left,
            right,
            |(_, left_value), (_, right_value)| match (left_value, right_value) {
                (Value::String(left), Value::String(right)) => {
                    let mut result = String::with_capacity(left.len() + right.len());
                    result.push_str(left);
                    result.push_str(right);
                    Some(Value::String(Rc::new(result)))
                }
                _ => None,
            },
        ),

        Value::Call(function, arguments) => fold_call(*function, arguments, values, functions),

        Value::Unit
        | Value::Int(_)
        | Value::String(_)
        | Value::Function(_, _)
        | Value::Phi(_)
        | Value::Arg(_) => None,
    };
    if let Some(value) = folded {
        values[value_id] = value;
    }
}

fn fold_binary(
    values: &mut ValueStorage,
    functions: &FunctionMap,
    left: ValueId,
    right: ValueId,
    fold: impl FnOnce((ValueId, &Value), (ValueId, &Value)) -> Option<Value>,
) -> Option<Value> {
    fold_constants(values, functions, left);
    fold_constants(values, functions, right);
    fold((left, &values[left]), (right, &values[right]))
}

fn fold_call(
    function: ValueId,
    arguments: &[ValueId],
    values: &ValueStorage,
    functions: &FunctionMap,
) -> Option<Value> {
    let arguments_are_constant = arguments
        .iter()
        .all(|argument| values[*argument].is_constant());
    if arguments_are_constant {
        let mut function_cfg = match &values[function] {
            Value::Function(fn_id, _) => functions[fn_id].clone(),
            _ => unreachable!(),
        };
        for (parameter_value_id, argument_value_id) in function_cfg.parameters.iter().enumerate() {
            function_cfg.values[*argument_value_id] = values[arguments[parameter_value_id]].clone();
        }

        fold_constants(&mut function_cfg.values, functions, function_cfg.result);
        let result_value = &function_cfg.values[function_cfg.result];
        if result_value.is_constant() {
            return Some(result_value.clone());
        }
    }
    None
}
