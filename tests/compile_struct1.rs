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
    #[enum_gen_match_id(Payload)]
    pub fn default(id: usize) -> Payload {
        EnumVariantType(EnumStructType::default())
    }

    #[enum_gen_match_id(Payload)]
    pub fn size_of(id: usize) -> usize {
        std::mem::size_of::<EnumStructType>()
    }

    #[enum_gen_match_self(Payload)]
    pub fn size(&self) -> usize {
        std::mem::size_of_val(inner)
    }

    #[enum_gen_match_self(Payload)]
    pub fn consume(self) {
        println!("I'm going away! {:?}", &inner)
    }

    fn set_first_byte<T: Sized>(p: &mut T) {
        let len = core::mem::size_of::<T>();
        if len > 0 {
            unsafe {
                let slice = core::slice::from_raw_parts_mut((p as *mut T) as *mut u8, len);
                slice[0] = 0;
            }
        }
    }

    #[enum_gen_match_self(Payload)]
    pub fn memset(&mut self) {
        Payload::set_first_byte(inner);
    }
}
