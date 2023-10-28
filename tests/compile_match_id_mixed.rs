/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use genmatch::*;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Hello {
    pub a: u8,
    pub b: u64,
    pub c: u64,
    pub d: u8,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Goodbye {
    pub a: u8,
    pub e: u8,
}

impl Goodbye {
    const ID: usize = 0x42;
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Invalid {}

#[genmatch]
#[derive(Debug, Eq, PartialEq)]
pub enum Payload {
    #[attr(ID = 0x2b)]
    Hello(Hello),
    Goodbye(Goodbye),
    #[attr(ID = _)]
    Invalid(Invalid),
}

impl Payload {
    #[genmatch_id(Payload)]
    pub fn default(id: usize) -> Payload {
        EnumVariantType(EnumStructType::default())
    }
}

#[test]
fn main() {
    assert_eq!(Payload::Hello(Hello::default()), Payload::default(0x2b));
}