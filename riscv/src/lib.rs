pub mod decoder;

pub type InstructionByteOrder = byteorder::LittleEndian;
pub type DataByteOrder = byteorder::LittleEndian;

pub mod registers {
    pub const REGISTER_COUNT: usize = 32;

    pub const ZERO: u8 = 0;
    pub const LINK: u8 = 1;
    pub const STACK_POINTER: u8 = 2;
    pub const GLOBAL_POINTER: u8 = 3;
    pub const ALTERNATE_LINK: u8 = 5;
    pub const TMP0: u8 = 5;
    pub const TMP1: u8 = 6;
    pub const TMP2: u8 = 7;
    pub const FRAME_POINTER: u8 = 8;
    pub const SAVED0: u8 = 8;
    pub const SAVED1: u8 = 9;
    pub const RETURN_VALUE0: u8 = 10;
    pub const RETURN_VALUE1: u8 = 11;
    pub const ARGUMENT0: u8 = 10;
    pub const ARGUMENT1: u8 = 11;
    pub const ARGUMENT2: u8 = 12;
    pub const ARGUMENT3: u8 = 13;
    pub const ARGUMENT4: u8 = 14;
    pub const ARGUMENT5: u8 = 15;
    pub const ARGUMENT6: u8 = 16;
    pub const ARGUMENT7: u8 = 17;
    pub const SAVED2: u8 = 18;
    pub const SAVED3: u8 = 19;
    pub const SAVED4: u8 = 20;
    pub const SAVED5: u8 = 21;
    pub const SAVED6: u8 = 22;
    pub const SAVED7: u8 = 23;
    pub const SAVED8: u8 = 24;
    pub const SAVED9: u8 = 25;
    pub const SAVED10: u8 = 26;
    pub const SAVED11: u8 = 27;
    pub const TMP3: u8 = 28;
    pub const TMP4: u8 = 29;
    pub const TMP5: u8 = 30;
    pub const TMP6: u8 = 31;

    pub const TEMPORARIES: &[u8] = &[TMP0, TMP1, TMP2, TMP3, TMP4, TMP5, TMP6];
    pub const SAVED: &[u8] = &[
        SAVED0, SAVED1, SAVED2, SAVED3, SAVED4, SAVED5, SAVED6, SAVED7, SAVED8, SAVED9, SAVED10,
        SAVED11,
    ];
    pub const RETURN_VALUES: &[u8] = &[RETURN_VALUE0, RETURN_VALUE1];
    pub const ARGUMENTS: &[u8] = &[
        ARGUMENT0, ARGUMENT1, ARGUMENT2, ARGUMENT3, ARGUMENT4, ARGUMENT5, ARGUMENT6, ARGUMENT7,
    ];
}
