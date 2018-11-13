use lazy_static::lazy_static;

use crate::vm::Instruction;
use crate::vm::Instruction::*;
use crate::vm::Register::*;

lazy_static! {
    pub(crate) static ref MEMCPY: Vec<Instruction> = {
        let (code, _) = asm!(vec![],
            Move(SP, R0) // frame pointer
            Push(R1)
            Push(R2)
            Push(R3)
            Push(R7)
            Const(0, R7)
            AddImm(R0, 4, R2) // fp + 1 -> size
            Load32(R2, R2)    // *[size] -> size
            AddImm(R0, 8, R1) // fp + 2 -> dst
            Load32(R1, R1)    // *[dst] -> dst
            AddImm(R0, 12, R0) // fp + 3 -> src
            Load32(R0, R0)    // *[src] -> src
        );
        let (code, loop_) = asm!(code,
            // loop:
            Debug // patch -> BrEqRel(size, 0, quit)
            Load8(R0, R3) // *[src] -> tmp
            Store8(R3, R1) // tmp -> *[dst]
            AddImm(R0, 1, R0) // src += 1
            AddImm(R1, 1, R1) // dst += 1
            SubImm(R2, 1, R2) // size -= 1
        );
        let (code, goto_loop) = asm!(code,
            Debug // patch -> BrRel(loop)
        );
        let (mut code, quit) = asm!(code,
            Pop(R7)
            Pop(R3)
            Pop(R2)
            Pop(R1)
            Ret
        );
        code[loop_ as usize] = BrEqRel(R2, R7, (quit - loop_) as i8);
        code[goto_loop as usize] = BrRel(-((goto_loop - loop_) as i8));
        code
    };
}
