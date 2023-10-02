/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use enum_parse::*;
use zerocopy::{AsBytes, FromBytes, FromZeroes};

const HELLO_PACKET_ID: usize = 0xAA;

#[enum_parse(derive(Debug, Default, FromBytes, AsBytes, FromZeroes),
             repr(C, packed),
             attr(parse_fn = read_from))]
pub enum Payload {
    #[attr(ID = HELLO_PACKET_ID)]
    Hello { a: u8, b: u64, c: u64, d: u8 },
    #[attr(ID = 0x42)]
    Goodbye { a: u8, e: u8 },
    #[attr(ID = _)]
    Invalid { some_always_present_byte: u8 },
}

#[test]
fn compile_struct2() {
    assert_eq!(Hello::ID, HELLO_PACKET_ID);
}