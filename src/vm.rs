use std::convert::From;
use std::error::Error;
use std::fmt;
use std::io::Write;
use std::mem::size_of;
use std::ops::Deref;
use std::ops::Range;

use itertools::Itertools;
use strum::IntoEnumIterator;
use strum_macros::*;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::utils::stdout;
use crate::vm::Instruction::*;
use crate::vm::Register::*;

macro_rules! asm {
    ($code: expr, $($instructions: expr)*) => ({
        let mut code: Vec<Instruction> = $code;
        let index = code.len() as u32;
        $(code.push($instructions);)*
        (code, index)
    })
}

fn asm(mut code: Vec<Instruction>, instructions: &[Instruction]) -> (Vec<Instruction>, u32) {
    let index = code.len() as u32;
    code.extend_from_slice(instructions);
    (code, index)
}

const RAM_SIZE: usize = 1024 * 1024;
const STACK_SIZE: usize = 8 * 1024;

struct DebugPrint(bool);

struct VM {
    registers: [u32; REGISTER_COUNT],
    rodata: Vec<u8>,
    ram: Vec<u8>,
}

impl VM {
    fn new() -> Self {
        let mut vm = Self {
            registers: [0; REGISTER_COUNT],
            rodata: vec![],
            ram: vec![0; RAM_SIZE],
        };
        vm.reset_sp();
        vm
    }

    fn run(&mut self, stdout: &mut StandardStream, program: &Program, DebugPrint(debug_print): DebugPrint) -> Result<(), Box<dyn Error>> {
        let executor = Executor {
            vm: self,
            program,
            stdout,
            debug_print
        };
        executor.run()
    }

    fn reset_sp(&mut self) {
        *self.reg_mut(SP) = RAM_SIZE as u32;
    }

    fn reg(&self, r: Register) -> &u32 {
        &self.registers[r as usize]
    }

    fn reg_mut(&mut self, r: Register) -> &mut u32 {
        &mut self.registers[r as usize]
    }

    fn ram<T>(&self, address: u32) -> Result<&T, RamAccessError> {
        let offset = self.ram_offset::<T>(address)?;
        Ok(unsafe { &*(&self.ram[offset] as *const u8 as *const T) })
    }

    fn ram_mut<T>(&mut self, address: u32) -> Result<&mut T, RamAccessError> {
        let offset = self.ram_offset::<T>(address)?;
        Ok(unsafe { &mut *(&mut self.ram[offset] as *mut u8 as *mut T) })
    }

    fn ram_offset<T>(&self, address: u32) -> Result<usize, RamAccessError> {
        let offset = address as usize;
        if offset <= RAM_SIZE - size_of::<T>() {
            Ok(offset)
        } else {
            Err(RamAccessError(address))
        }
    }

    fn dump_registers(&mut self, stdout: &mut StandardStream) -> Result<(), Box<dyn Error>> {
        for (i, register) in Register::iter().enumerate() {
            self.dump_register(stdout, &register, self.registers[i])?;
        }
        Ok(())
    }

    fn dump_register(&mut self, stdout: &mut StandardStream, register: &Register, value: u32) -> Result<(), std::io::Error> {
        writeln!(stdout, "{}: {:08x} ({})", register.as_ref(), value, value)
    }

    #[allow(unused)]
    fn dump_mem(&mut self, stdout: &mut StandardStream, range: Range<usize>) -> Result<(), std::io::Error> {
        stdout.set_color(ColorSpec::new()
            .set_fg(Some(Color::Yellow))
            .set_bold(true))?;
        write!(stdout, "memory ")?;
        stdout.reset()?;
        writeln!(stdout, "{:08x}..{:08x}:", range.start, range.end)?;

        const BLOCK_SIZE: usize = 16;
        let start = range.start / BLOCK_SIZE;
        let end = range.end / BLOCK_SIZE + ((range.end % BLOCK_SIZE) & 1);
        for (i, block) in (self.ram[start * BLOCK_SIZE..]).chunks(BLOCK_SIZE).enumerate().take(end - start) {
            write!(stdout, "{:08x}  ", i * BLOCK_SIZE)?;
            writeln!(stdout, "{:02x}", block.iter().format(" "))?;
        }
        Ok(())
    }

    fn dump_stack(&mut self, stdout: &mut StandardStream) -> Result<(), Box<dyn Error>> {
        stdout.set_color(ColorSpec::new()
            .set_fg(Some(Color::Yellow))
            .set_bold(true))?;
        writeln!(stdout, "stack:")?;
        stdout.reset()?;
        let mut address = RAM_SIZE as u32 - 4;
        let sp = *self.reg(SP);
        while address >= sp {
            let value = *self.ram::<u32>(address)?;
            writeln!(stdout, "[{:08x}] {:08x} ({})", address, value, value)?;
            address -= 4;
        }
        Ok(())
    }

    fn push(&mut self, value: u32) -> Result<(), Box<dyn Error>> {
        let mut sp = *self.reg(SP);
        if (sp as usize) <= RAM_SIZE - STACK_SIZE {
            return Err(From::from(StackOverflow(sp)));
        }
        sp -= 4;
        *self.reg_mut(SP) = sp;
        *self.ram_mut(sp)? = value;
        Ok(())
    }

    fn pop(&mut self) -> Result<u32, Box<dyn Error>> {
        let sp = *self.reg(SP);
        if (sp as usize) >= RAM_SIZE {
            return Err(From::from(StackUnderflow(sp)));
        }
        *self.reg_mut(SP) += 4;
        Ok(*self.ram(sp)?)
    }

    fn load<T: Into<u32> + Copy>(&mut self, address: u32, register: Register) -> Result<(), RamAccessError> {
        *self.reg_mut(register) = self.ram::<T>(address).map(|value| (*value).into())?;
        Ok(())
    }
}

struct Executor<'vm, 'program, 'stdout> {
    vm: &'vm mut VM,
    program: &'program Program,
    stdout: &'stdout mut StandardStream,
    debug_print: bool
}

impl<'vm, 'program, 'stdout> Executor<'vm, 'program, 'stdout> {
    fn run(mut self) -> Result<(), Box<dyn Error>> {
        self.vm.rodata = self.program.rodata.clone();
        let program_length = self.program.code.len() as u32;
        *self.vm.reg_mut(PC) = 0;
        if self.debug_print {
            self.dump_code()?;
            self.vm.dump_registers(self.stdout)?;
            self.vm.dump_stack(self.stdout)?;
        }
        while *self.vm.reg(PC) < program_length {
            let instruction = self.current_instruction();
            if self.debug_print {
                self.dump_instruction(*self.vm.reg(PC), instruction)?;
            }
            let pc = *self.vm.reg(PC);
            *self.vm.reg_mut(PC) += 1;
            match instruction {
                Stop => break,
                Debug => self.vm.dump_registers(self.stdout)?,
                LoadImmediate(value, dst) => *self.vm.reg_mut(dst) = value,
                LoadIndirect8(src, dst) => self.vm.load::<u8>(*self.vm.reg(src), dst)?,
                LoadIndirect16(src, dst) => self.vm.load::<u16>(*self.vm.reg(src), dst)?,
                LoadIndirect32(src, dst) => self.vm.load::<u32>(*self.vm.reg(src), dst)?,
                Store8(src, dst) => {
                    let address = *self.vm.reg(dst);
                    *self.vm.ram_mut(address)? = *self.vm.reg(src) as u8;
                },
                Store16(src, dst) => {
                    let address = *self.vm.reg(dst);
                    *self.vm.ram_mut(address)? = *self.vm.reg(src) as u16;
                },
                Store32(src, dst) => {
                    let address = *self.vm.reg(dst);
                    *self.vm.ram_mut(address)? = *self.vm.reg(src) as u32;
                },
                Move(src, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(src),
                Push(register) => self.vm.push(*self.vm.reg(register))?,
                Pop(register) => *self.vm.reg_mut(register) = self.vm.pop()?,
                Br(dst) => self.br(dst)?,
                BrRel(offset) => self.br_rel(pc, offset)?,
                BrEq(op1, op2, dst) => {
                    if self.equals(op1, op2) {
                        self.br(dst)?;
                    }
                },
                BrEqRel(op1, op2, offset) => {
                    if self.equals(op1, op2) {
                        self.br_rel(pc, offset)?;
                    }
                },
                Call(index) => self.call(*self.vm.reg(PC), index)?,
                CallIndirect(register) => self.call(*self.vm.reg(PC), *self.vm.reg(register))?,
                Ret => *self.vm.reg_mut(PC) = self.vm.pop()?,
                Add(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) + *self.vm.reg(op2),
                AddImm(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) + op2,
                Sub(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) - *self.vm.reg(op2),
                SubImm(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) - op2,
                Mul(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) * *self.vm.reg(op2),
                Div(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) / *self.vm.reg(op2),
                And(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) & *self.vm.reg(op2),
                Or(op1, op2, dst) => *self.vm.reg_mut(dst) = *self.vm.reg(op1) | *self.vm.reg(op2),
            }
            if self.debug_print {
                self.vm.dump_registers(self.stdout)?;
                self.vm.dump_stack(self.stdout)?;
            }
        }
        Ok(())
    }

    fn equals(&self, op1: Register, op2: Register) -> bool {
        *self.vm.reg(op1) == *self.vm.reg(op2)
    }

    fn dump_code(&mut self) -> Result<(), std::io::Error> {
        let code = self.program.code.iter().cloned().enumerate().collect::<Vec<_>>();
        for (i, instruction) in code {
            self.dump_instruction(i as u32, instruction)?;
        }
        Ok(())
    }

    fn dump_instruction(&mut self, pc: u32, instruction: Instruction) -> Result<(), std::io::Error> {
        self.stdout.set_color(ColorSpec::new()
            .set_fg(Some(Color::Green))
            .set_bold(true))?;
        write!(self.stdout, ">> ")?;
        self.stdout.reset()?;
        write!(self.stdout, "{:08x} ", pc)?;
        self.stdout.set_color(ColorSpec::new()
            .set_fg(Some(Color::White))
            .set_bold(true))?;
        writeln!(self.stdout, "{:?}", instruction)?;
        self.stdout.reset()
    }

    fn current_instruction(&self) -> Instruction {
        self.program.code[*self.vm.reg(PC) as usize]
    }

    fn br(&mut self, dst: u32) -> Result<(), BrRangeError> {
        if dst as usize >= self.program.code.len() {
            return Err(BrRangeError(dst));
        }
        *self.vm.reg_mut(PC) = dst;
        Ok(())
    }

    fn br_rel(&mut self, pc: u32, offset: i8) -> Result<(), BrRelRangeError> {
        let new_pc = i64::from(pc) + i64::from(offset);
        if new_pc < 0 || new_pc >= self.program.code.len() as i64 {
            return Err(BrRelRangeError(pc, offset));
        }
        *self.vm.reg_mut(PC) = new_pc as u32;
        Ok(())
    }

    fn call(&mut self, pc: u32, procedure: u32) -> Result<(), Box<dyn Error>> {
        self.vm.push(pc + 1)?;
        *self.vm.reg_mut(PC) = procedure;
        Ok(())
    }
}

struct Program {
    rodata: Vec<u8>,
    code: Vec<Instruction>
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Address(u32);

impl Deref for Address {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Copy, Clone, Debug)]
#[allow(unused)]
pub(crate) enum Instruction {
    Stop,
    Debug,
    LoadImmediate(u32, Register),
    LoadIndirect8(Register, Register),
    LoadIndirect16(Register, Register),
    LoadIndirect32(Register, Register),
    Store8(Register, Register),
    Store16(Register, Register),
    Store32(Register, Register),
    Move(Register, Register),
    Push(Register),
    Pop(Register),
    Br(u32),
    BrRel(i8),
    BrEq(Register, Register, u32),
    BrEqRel(Register, Register, i8),
    Call(u32),
    CallIndirect(Register),
    Ret,
    // op1, op2, dst
    Add(Register, Register, Register),
    AddImm(Register, u32, Register),
    Sub(Register, Register, Register),
    SubImm(Register, u32, Register),
    Mul(Register, Register, Register),
    Div(Register, Register, Register),
    And(Register, Register, Register),
    Or(Register, Register, Register),
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, EnumCount, EnumIter, AsRefStr)]
pub(crate) enum Register {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    SP,
    PC
}

#[derive(Debug)]
struct RamAccessError(u32);

impl Error for RamAccessError {}

impl fmt::Display for RamAccessError  {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "RAM address out of range: {:08x} ({})", self.0, self.0)
    }
}

#[derive(Debug)]
struct StackOverflow(u32);

impl Error for StackOverflow {}

impl fmt::Display for StackOverflow  {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "stack overflow: SP = {:08x} ({})", self.0, self.0)
    }
}

#[derive(Debug)]
struct StackUnderflow(u32);

impl Error for StackUnderflow {}

impl fmt::Display for StackUnderflow  {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "stack underflow: SP = {:08x} ({})", self.0, self.0)
    }
}

#[derive(Debug)]
struct BrRangeError(u32);

impl Error for BrRangeError {}

impl fmt::Display for BrRangeError  {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "jump target out of range: pc = {:08x}", self.0)
    }
}

#[derive(Debug)]
struct BrRelRangeError(u32, i8);

impl Error for BrRelRangeError {}

impl fmt::Display for BrRelRangeError  {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "jump target out of range: pc = {:08x}, offset = {}", self.0, self.1)
    }
}

#[test]
pub(crate) fn vm() -> Result<(), Box<dyn Error>> {
    let stdout = &mut stdout();

    let mut vm = VM::new();
    for i in 0..8 {
        *vm.ram_mut::<u8>(u32::from(i))? = 0xf0 + i;
    }
    let (code, start) = asm!(vec![],
        Debug // patch -> jump over MEMCPY
    );
    let (code, memcpy) = asm(code, &crate::runtime::MEMCPY);
    let (mut code, entry) = asm!(code,
        Call(memcpy)
        Stop
    );
    code[start as usize] = Br(entry as u32);
    let program = Program {
        rodata: vec![],
        code,
    };
    vm.reset_sp();
    let src = 0;
    let dst = 0x70;
    vm.push(src)?;
    vm.push(dst)?;
    vm.push(8)?;
    vm.run(stdout, &program, DebugPrint(false))?;
    assert_eq!(*vm.ram::<u64>(src)?, *vm.ram::<u64>(dst)?);
    Ok(())
}
