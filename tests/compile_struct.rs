/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use enum_parse::*;
use zerocopy::{AsBytes, FromBytes, FromZeroes};

#[enum_parse(derive(Debug, FromBytes, AsBytes, FromZeroes),
             repr(C, packed),
             attr(parse_fn = read_from))]
pub enum Payload {
    Hello { a: u8, b: u64, c: u64, d: u8 },
    Goodbye { a: u8, e: u8 },
}

#[test]
fn impl_test() {
    let _ = Hello::ID;
}