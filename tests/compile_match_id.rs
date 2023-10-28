/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use genmatch::*;

#[derive(Debug, Default)]
pub struct Hello {
    pub a: u8,
    pub b: u64,
    pub c: u64,
    pub d: u8,
}

#[derive(Debug, Default)]
pub struct Goodbye {
    pub a: u8,
    pub e: u8,
}

#[derive(Debug, Default)]
pub struct Invalid {}

#[genmatch]
#[derive(Debug)]
pub enum Payload {
    #[attr(ID = 0x2b)]
    Hello(Hello),
    #[attr(ID = 0x42)]
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
