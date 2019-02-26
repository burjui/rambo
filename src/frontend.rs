/*
The minimal SSA algorithm is based on the following paper:

Braun M., Buchwald S., Hack S., Leißa R., Mallon C., Zwinkau A.
(2013) Simple and Efficient Construction of Static Single Assignment Form.
In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
*/

use std::collections::HashMap;
use std::collections::HashSet;
#[cfg(test)] use std::fs::File;
use std::iter::empty;
use std::iter::once;
#[cfg(test)] use std::rc::Rc;

use itertools::free::chain;
use itertools::Itertools;
#[cfg(test)] use num_bigint::BigInt;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::frontend::dead_code::remove_dead_code;
#[cfg(test)] use crate::graphviz::Graphviz;
use crate::ir::BasicBlock;
use crate::ir::BasicBlockGraph;
use crate::ir::ControlFlowGraph;
use crate::ir::FnId;
use crate::ir::IdentGenerator;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueIndex;
use crate::ir::value_storage::ValueStorage;
use crate::ir::Variable;
use crate::ir::VarId;
use crate::semantics::BindingKind;
use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use crate::unique_rc::UniqueRc;
#[cfg(test)] use crate::utils::TestResult;

mod dead_code;

pub(crate) struct FrontEnd {
    name: String,
    enable_warnings: bool,
    include_comments: bool,
    graph: BasicBlockGraph,
    idgen: IdentGenerator<VarId>,
    function_idgen: IdentGenerator<FnId>,
    variables: HashMap<Variable, HashMap<NodeIndex, VarId>>,
    variables_read: HashSet<Variable>,
    values: ValueStorage,
    definitions: HashMap<VarId, IdentDefinition>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<Variable, VarId>>,
    phi_users: HashMap<VarId, HashSet<VarId>>,
    entry_block: NodeIndex,
    undefined: VarId,
    undefined_users: Vec<StatementLocation>,
    markers: Vec<Option<Marker>>,
}

impl FrontEnd {
    pub(crate) fn new(name: &str) -> Self {
        let mut instance = Self {
            name: name.to_owned(),
            enable_warnings: false,
            include_comments: false,
            graph: BasicBlockGraph::new(),
            idgen: IdentGenerator::new(),
            function_idgen: IdentGenerator::new(),
            variables: HashMap::new(),
            variables_read: HashSet::new(),
            values: ValueStorage::new(),
            definitions: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_users: HashMap::new(),
            entry_block: NodeIndex::new(0),
            undefined: VarId::default(),
            undefined_users: Vec::new(),
            markers: Vec::new(),
        };
        instance.entry_block = instance.new_block();
        instance.seal_block(instance.entry_block);
        instance.undefined = instance.define(instance.entry_block, Value::Undefined);
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

    pub(crate) fn build(mut self, code: &ExprRef) -> ControlFlowGraph {
        let (exit_block, program_result) = self.process_expr(code, self.entry_block);
        self.push_return(exit_block, program_result);
        self.assert_no_undefined_variables();

        if self.enable_warnings {
            let variables = self.variables
                .into_iter()
                .map(|(variable, _)| variable)
                .collect::<HashSet<_>>();
            let redundant_bindings = variables
                .difference(&self.variables_read)
                .filter_map(|variable| match variable {
                    Variable::Binding(binding) => Some(binding.clone()),
                    Variable::Value(_) => None,
                });
            warn_about_redundant_bindings(redundant_bindings);
        }

        remove_dead_code(
            self.entry_block, program_result, &mut self.values, &mut self.graph, self.definitions);

        ControlFlowGraph {
            name: self.name,
            graph: self.graph,
            entry_block: self.entry_block,
            exit_block,
            undefined: self.undefined,
            values: self.values,
        }
    }

    fn assert_no_undefined_variables(&self) {
        assert!(self.undefined_users.is_empty(), "found undefined usages:\n{:?}\n",
                self.undefined_users.iter()
                    .map(|user| &self.graph[user.block][user.index])
                    .format("\n"));
    }

    fn process_expr(&mut self, expr: &ExprRef, block: NodeIndex) -> (NodeIndex, VarId) {
        match &**expr {
            TypedExpr::Block(block_data) => {
                let mut current_block = block;
                let mut current_ident = None;
                for statement in &block_data.statements {
                    let (block, ident) = self.process_statement(statement, current_block);
                    current_block = block;
                    current_ident = Some(ident);
                }
                let ident = current_ident.unwrap_or_else(|| self.define(block, Value::Unit));
                (current_block, ident)
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
                (block, self.read_variable(&Variable::Binding(binding.clone()), block))
            }

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                self.comment(block, source.text());
                let (block, condition) = self.process_expr(condition, block);
                let positive_block = self.new_block();
                let negative_block = self.new_block();

                let statement_location = StatementLocation::new(block, self.graph[block].len());
                self.record_phi_and_undefined_usages(statement_location, condition);
                self.graph[block].push(
                    Statement::CondJump(condition, positive_block, negative_block));

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
                let variable = Variable::Binding(binding.clone());
                let (block, ident) = self.process_expr(value, block);
                self.write_variable(&variable, block, ident);
                (block, ident)
            }

            TypedExpr::Lambda(lambda, source) => {
                let mut function = FrontEnd::new(source.text())
                    .enable_warnings(self.enable_warnings)
                    .include_comments(self.include_comments);
                let function_id = self.function_idgen.next_id();
                for parameter in &lambda.parameters {
                    function.comment(function.entry_block, &parameter.name);
                    let index = if let BindingKind::Arg(index) = parameter.kind {
                        index
                    } else {
                        unreachable!()
                    };
                    let declaration = function.define(function.entry_block, Value::Arg(index));
                    function.write_variable(&Variable::Binding(parameter.clone()), function.entry_block, declaration);
                }
                let function_cfg = UniqueRc::from(function.build(&lambda.body));
                (block, self.define(block, Value::Function(function_id, function_cfg)))
            }

            TypedExpr::Application { function, arguments, source, .. } => {
                self.comment(block, source.text());
                let (block, function_ident) = self.process_expr(function, block);
                let mut current_block = block;
                let mut argument_idents = vec![];
                for argument in arguments {
                    let (block, ident) = self.process_expr(argument, current_block);
                    argument_idents.push(ident);
                    current_block = block;
                }
                (block, self.define(current_block, Value::Call(function_ident, argument_idents)))
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, block: NodeIndex, left: &ExprRef, right: &ExprRef, source: &Source,
                      value_constructor: impl Fn(VarId, VarId) -> Value) -> (NodeIndex, VarId)
    {
        let (block, left_ident) = self.process_expr(left, block);
        let (block, right_ident) = self.process_expr(right, block);
        self.comment(block, source.text());
        (block, self.define(block, value_constructor(left_ident, right_ident)))
    }

    fn process_statement(&mut self, statement: &TypedStatement, block: NodeIndex) -> (NodeIndex, VarId) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, ident) = self.process_expr(&binding.data, block);
                self.write_variable(&Variable::Binding(binding.clone()), block, ident);
                (block, ident)
            },
            TypedStatement::Expr(expr) => {
                self.process_expr(expr, block)
            }
        }
    }

    fn write_variable(&mut self, variable: &Variable, block: NodeIndex, ident: VarId) {
        self.variables
            .entry(variable.clone())
            .or_default()
            .insert(block, ident);
    }

    fn read_variable(&mut self, variable: &Variable, block: NodeIndex) -> VarId {
        self.reset_markers();
        self.read_variable_core(variable, block)
    }

    fn read_variable_core(&mut self, variable: &Variable, block: NodeIndex) -> VarId {
        self.variables_read.insert(variable.clone());
        self.variables
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &Variable, block: NodeIndex) -> VarId {
        let ident = if !self.sealed_blocks.contains(&block) {
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
                    0 => self.undefined,
                    _ => {
                        if definitions.contains(&self.undefined) {
                            self.undefined
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
        self.write_variable(variable, block, ident);
        ident
    }

    fn add_phi_operands(&mut self, variable: &Variable, phi: VarId) -> VarId {
        let predecessors = self.graph
            .edges_directed(self.definitions[&phi].location.block, Direction::Incoming)
            .map(|edge| edge.source())
            .collect_vec();
        for predecessor in predecessors {
            let operand = self.read_variable_core(variable, predecessor);
            self.phi_operands_mut(phi).push(operand);
        }
        self.phi_operands_mut(phi).sort();
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: VarId) -> VarId {
        let mut same = None;
        for &operand in self.phi_operands(phi) {
            if operand == self.undefined {
                same = Some(self.undefined);
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
            Some(ident) => ident,
            None => self.undefined
        };

        self.phi_users.get_mut(&phi).unwrap().remove(&phi);
        let mut possible_trivial_phis = HashSet::new();
        for user in &self.phi_users[&phi] {
            let value_index = self.definitions[user].value_index;
            let value = &mut self.values[value_index];
            replace_ident(value, phi, same);
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

    fn define(&mut self, block: NodeIndex, value: Value) -> VarId {
        let (value_index, reused) = self.values.insert(value);
        let variable = Variable::Value(value_index);
        let ident = if *reused {
            self.read_variable(&variable, block)
        } else {
            self.undefined
        };
        if ident != self.undefined {
            ident
        } else {
            let ident = self.idgen.next_id();
            let basic_block = &mut self.graph[block];
            let basic_block_len = basic_block.len();
            let statement_index = match basic_block.last() {
                Some(Statement::CondJump(_, _, _)) => basic_block_len - 1,
                _ => basic_block_len,
            };
            let location = StatementLocation::new(block, statement_index);
            basic_block.insert(statement_index, Statement::Definition {
                ident,
                value_index,
            });
            self.write_variable(&Variable::Value(value_index), block, ident);
            self.definitions.insert(ident, IdentDefinition { value_index, location });
            self.record_phi_and_undefined_usages(location, ident);
            if let Value::Phi(_) = &self.values[value_index] {
                self.phi_users.insert(ident, HashSet::new());
            }
            ident
        }
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[VarId]) -> VarId {
        let mut operands = operands.to_vec();
        operands.sort();
        self.define(block, Value::Phi(Phi(operands)))
    }

    fn push_return(&mut self, block: NodeIndex, ident: VarId) {
        self.graph[block].push(Statement::Return(ident));
    }

    fn record_phi_and_undefined_usages(&mut self, location: StatementLocation, ident: VarId) {
        let value_index = self.definitions[&ident].value_index;
        let value = &self.values[value_index];
        let (is_using_undefined, phis_used) = get_value_ident_operands(value)
            .fold((false, vec![]), |(mut is_using_undefined, mut phis_used), ident| {
                if ident == self.undefined {
                    is_using_undefined = true;
                } else if self.is_a_phi(ident) {
                    phis_used.push(ident);
                }
                (is_using_undefined, phis_used)
            });
        if is_using_undefined {
            self.undefined_users.push(location);
        }
        for phi in phis_used {
            self.phi_users.get_mut(&phi).unwrap().insert(ident);
        }
    }

    fn is_a_phi(&self, ident: VarId) -> bool {
        ident != self.undefined && self.phi_users.contains_key(&ident)
    }

    fn phi_operands(&self, phi: VarId) -> &[VarId] {
        let value_index = self.definitions[&phi].value_index;
        match &self.values[value_index] {
            Value::Phi(Phi(operands)) => operands,
            _ => unreachable!()
        }
    }

    fn phi_operands_mut(&mut self, phi: VarId) -> &mut Vec<VarId> {
        let value_index = self.definitions[&phi].value_index;
        match &mut self.values[value_index] {
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

fn warn_about_redundant_bindings(redundant_bindings: impl Iterator<Item = BindingRef>) {
    let redundant_bindings = redundant_bindings
        .sorted_by(|a, b| a.source.range().start().cmp(&b.source.range().start()));
    for binding in redundant_bindings {
        warning_at!(binding.source, "unused definition: {}", binding.source.text());
    }
}

fn get_statement_ident_operands<'a>(values: &'a ValueStorage, statement: &'a Statement) -> Box<dyn Iterator<Item = VarId> + 'a> {
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition { value_index, .. } => get_value_ident_operands(&values[*value_index]),
        Statement::CondJump(condition, _, _) => Box::new(once(*condition)),
        Statement::Return(result) => Box::new(once(*result)),
    }
}

fn get_statement_idents_mut<'a>(values: &'a mut ValueStorage, statement: &'a mut Statement) -> Box<dyn Iterator<Item = &'a mut VarId> + 'a> {
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition { ident, value_index } => Box::new(once(ident).chain(get_value_ident_operands_mut(&mut values[*value_index]))),
        Statement::CondJump(condition, _, _) => Box::new(once(condition)),
        Statement::Return(result) => Box::new(once(result)),
    }
}

fn replace_ident(value: &mut Value, ident: VarId, replacement: VarId) {
    let operands: Box<dyn Iterator<Item = &mut VarId>> = match value {
        Value::Undefined |
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(left).chain(once(right))),
        Value::Phi(operands) => Box::new(operands.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
    };
    for operand in operands {
        if *operand == ident {
            *operand = replacement;
        }
    }
}

fn get_value_ident_operands<'a>(value: &'a Value) -> Box<dyn Iterator<Item = VarId> + 'a> {
    match value {
        Value::Undefined |
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(*left).chain(once(*right))),

        Value::Phi(phi) => Box::new(phi.iter().cloned()),
        Value::Call(function, arguments) => Box::new(once(*function).chain(arguments.iter().cloned())),
    }
}

fn get_value_ident_operands_mut<'a>(value: &'a mut Value) -> Box<dyn Iterator<Item = &mut VarId> + 'a> {
    match value {
        Value::Undefined |
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(left).chain(once(right))),

        Value::Phi(phi) => Box::new(phi.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
    }
}

#[derive(Copy, Clone)]
enum Marker {
    Initial,
    Phi(VarId),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct StatementLocation {
    block: NodeIndex,
    index: usize
}

impl StatementLocation {
    fn new(block: NodeIndex, index: usize) -> Self {
        Self { block, index }
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct IdentDefinition {
    value_index: ValueIndex,
    location: StatementLocation,
}

macro_rules! test_frontend {
    ($name: ident, $code: expr, $expected_result: expr) => {
        #[test]
        fn $name() -> TestResult {
            let code = typecheck!($code)?;
            let cfg = FrontEnd::new(&location!())
                .enable_warnings(false)
                .include_comments(false)
                .build(&code);
            let mut output = File::create("ir_cfg.dot")?;
            Graphviz::write(&mut output, &cfg, cfg.name.clone())?;

            let value = crate::ir::eval::EvalContext::new(&cfg).eval();
            assert_eq!(value, $expected_result);

            Ok(())
        }
    }
}

test_frontend! {
    generic,
    "
    let x = 47
    let y = 29
    let z = y
    1
    let r = if 0 {
        let a = z
        let b = a
        z = 22
        x + b + y
        1
    } else {
        z = 33
        1
    }
    z
    r + 1
    z + 1

    z = 6
    z + 7
    z = 8
    z + 9

    let f = λ (g: λ (u:str, v:str) -> str, u:str, v:str) -> g u v
    let s1 = f (λ (u:str, v:str) -> \"(\" + u + \"; \" + v + \")\") \"hello\" \"world\"
    let s2 = f (λ (u:str, v:str) -> \"<\" + u + \"; \" + v + \">\") (\"bye, \" + \"world\") \"seeya\"
    let result = s1 + s2
    result
    ",
    Value::String(Rc::new("(hello; world)<bye, world; seeya>".to_owned()))
}

test_frontend! {
    block_removal,
    "
    let x = 0
    let y = 999
    if x {
        x = 1
    } else {
        x = 2
    }
    if y {
        x = 3
    } else {
        x = 4
    }
    x
    ",
    Value::Int(BigInt::from(3))
}

test_frontend! {
    block_removal2,
    &"
    let x = \"abc\"
    let y = \"efg\"
    let z = λ (a:str, b:str) -> {
        let result = a + b
        result
    }
    if 0 {
        \"false\"
    }
    else {
        let a = (z x y)
        z a \"/test\"
    }
    let nn = 1
    ".repeat(7),
    Value::Unit
}

test_frontend!{
    marker_eval,
    "
    let a = 1
    let b = 2
    (a + 1) * (b - 1)
    let f = λ (a: num, b: num) -> a + b
    let g = λ (f: λ (a: num, b: num) -> num, a: num, b: num) → f a b + 1
    g f 1 2
    (a + 1) * (b - 1)
    a = 10
    g f 1 2
    (a + 1) * (b - 1)
    a = 10
    ",
    Value::Int(BigInt::from(10))
}
