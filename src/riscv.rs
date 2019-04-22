use byteorder::LittleEndian;

pub(crate) type InstructionByteOrder = LittleEndian;
pub(crate) type DataByteOrder = LittleEndian;

pub(crate) mod registers {
    pub(crate) const REGISTER_COUNT: usize = 32;

    pub(crate) const ZERO: u8 = 0;
    pub(crate) const LINK: u8 = 1;
    pub(crate) const STACK_POINTER: u8 = 2;
    pub(crate) const GLOBAL_POINTER: u8 = 3;
    pub(crate) const ALTERNATE_LINK: u8 = 5;
    pub(crate) const TMP0: u8 = 5;
    pub(crate) const TMP1: u8 = 6;
    pub(crate) const TMP2: u8 = 7;
    pub(crate) const FRAME_POINTER: u8 = 8;
    pub(crate) const SAVED0: u8 = 8;
    pub(crate) const SAVED1: u8 = 9;
    pub(crate) const RETURN_VALUE0: u8 = 10;
    pub(crate) const RETURN_VALUE1: u8 = 11;
    pub(crate) const ARGUMENT0: u8 = 10;
    pub(crate) const ARGUMENT1: u8 = 11;
    pub(crate) const ARGUMENT2: u8 = 12;
    pub(crate) const ARGUMENT3: u8 = 13;
    pub(crate) const ARGUMENT4: u8 = 14;
    pub(crate) const ARGUMENT5: u8 = 15;
    pub(crate) const ARGUMENT6: u8 = 16;
    pub(crate) const ARGUMENT7: u8 = 17;
    pub(crate) const SAVED2: u8 = 18;
    pub(crate) const SAVED3: u8 = 19;
    pub(crate) const SAVED4: u8 = 20;
    pub(crate) const SAVED5: u8 = 21;
    pub(crate) const SAVED6: u8 = 22;
    pub(crate) const SAVED7: u8 = 23;
    pub(crate) const SAVED8: u8 = 24;
    pub(crate) const SAVED9: u8 = 25;
    pub(crate) const SAVED10: u8 = 26;
    pub(crate) const SAVED11: u8 = 27;
    pub(crate) const TMP3: u8 = 28;
    pub(crate) const TMP4: u8 = 29;
    pub(crate) const TMP5: u8 = 30;
    pub(crate) const TMP6: u8 = 31;

    pub(crate) const TEMPORARIES: &[u8] = &[ TMP0, TMP1, TMP2, TMP3, TMP4, TMP5, TMP6 ];
    pub(crate) const SAVED: &[u8] = &[
        SAVED0, SAVED1, SAVED2, SAVED3, SAVED4, SAVED5,
        SAVED6, SAVED7, SAVED8, SAVED9, SAVED10, SAVED11,
    ];
    pub(crate) const RETURN_VALUES: &[u8] = &[ RETURN_VALUE0, RETURN_VALUE1 ];
    pub(crate) const ARGUMENTS: &[u8] = &[
        ARGUMENT0, ARGUMENT1, ARGUMENT2, ARGUMENT3,
        ARGUMENT4, ARGUMENT5, ARGUMENT6, ARGUMENT7,
    ];
}
