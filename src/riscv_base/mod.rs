pub mod decoder;

#[allow(unused)]
pub mod registers {
    pub const REGISTER_COUNT: usize = 32;

    pub const ZERO: u8 = 0; // Hard-wired zero
    pub const RA: u8 = 1; // Return address
    pub const SP: u8 = 2; // Stack pointer
    pub const GP: u8 = 3; // Global pointer
    pub const TP: u8 = 4; // Thread pointer
    pub const FP: u8 = 8; // Saved register/frame pointer

    // Temporaries
    pub const T0: u8 = 5;
    pub const T1: u8 = 6;
    pub const T2: u8 = 7;
    pub const T3: u8 = 28;
    pub const T4: u8 = 29;
    pub const T5: u8 = 30;
    pub const T6: u8 = 31;

    // Function arguments/return values
    pub const A0: u8 = 10;
    pub const A1: u8 = 11;

    // Function arguments
    pub const A2: u8 = 12;
    pub const A3: u8 = 13;
    pub const A4: u8 = 14;
    pub const A5: u8 = 15;
    pub const A6: u8 = 16;
    pub const A7: u8 = 17;

    // Saved registers
    pub const S0: u8 = 8;
    pub const S1: u8 = 9;
    pub const S2: u8 = 18;
    pub const S3: u8 = 19;
    pub const S4: u8 = 20;
    pub const S5: u8 = 21;
    pub const S6: u8 = 22;
    pub const S7: u8 = 23;
    pub const S8: u8 = 24;
    pub const S9: u8 = 25;
    pub const S10: u8 = 26;
    pub const S11: u8 = 27;
}
