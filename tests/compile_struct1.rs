/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use enum_parse::*;
use zerocopy::{AsBytes, FromBytes, FromZeroes};

#[enum_parse(derive(Debug, Default, FromBytes, AsBytes, FromZeroes),
             repr(C, packed),
             attr(parse_input = &[u8], parse_fn = read_from))]
pub enum Payload {
    #[attr(ID = 0x2b)]
    Hello { a: u8, b: u64, c: u64, d: u8 },
    #[attr(ID = 0x42)]
    Goodbye { a: u8, e: u8 },
    #[attr(ID = _)]
    Invalid,
}

#[test]
fn compile_struct1() {
    assert_eq!(Hello::ID, 0x2b);
    assert_eq!(std::mem::size_of::<Hello>(), 18);
}