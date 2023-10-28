/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use enum_match::*;

#[derive(Debug, Default)]
#[repr(C, packed)]
pub struct Hello {
    pub a: u8,
    pub b: u64,
    pub c: u64,
    pub d: u8,
}

#[derive(Debug, Default)]
#[repr(C, packed)]
pub struct Goodbye {
    pub a: u8,
    pub e: u8,
}

#[derive(Debug, Default)]
#[repr(C, packed)]
pub struct Invalid {}

#[enum_match]
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
    #[enum_match_id(Payload)]
    pub fn default(id: usize) -> Payload {
        EnumVariantType(EnumStructType::default())
    }

    #[enum_match_id(Payload)]
    pub fn size_of(id: usize) -> usize {
        std::mem::size_of::<EnumStructType>()
    }

    #[enum_match_self(Payload)]
    pub fn size(&self) -> usize {
        std::mem::size_of_val(inner)
    }

    #[enum_match_self(Payload)]
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

    #[enum_match_self(Payload)]
    pub fn memset(&mut self) {
        Payload::set_first_byte(inner);
    }
}
