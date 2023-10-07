/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use enum_gen::*;

#[enum_gen(derive(Debug, Default), repr(C, packed))]
pub enum Payload {
    #[attr(ID = 0x2b)]
    Hello { a: u8, b: u64, c: u64, d: u8 },
    #[attr(ID = 0x42)]
    Goodbye { a: u8, e: u8 },
    #[attr(ID = _)]
    Invalid,
}

impl Payload {
    #[enum_gen_impl(Payload)]
    pub fn default(id: usize) -> Payload {
        EnumVariantType(EnumStructType::default())
    }

    #[enum_gen_impl(Payload)]
    pub fn size_of(id: usize) -> usize {
        std::mem::size_of::<EnumStructType>()
    }
}
