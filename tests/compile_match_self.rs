/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use enum_match::*;

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

#[enum_match]
#[derive(Debug)]
pub enum Payload {
    Hello(Hello),
    Goodbye(Goodbye),
    Invalid(Invalid),
}

impl Payload {
    #[enum_match_self(Payload)]
    pub fn size(&self) -> usize {
        std::mem::size_of_val(inner)
    }
}
