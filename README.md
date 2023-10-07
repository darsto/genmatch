# enum_gen

Provides the [`enum_gen`] procedural macro to generate structures from enum variants.
The variants can be assigned a numerical ID, which can be automatically matched by functions attributed with another proc macro #[`enum_gen_impl`] that is also provided by this crate.

This is best explained with an example.

## Example

```rust
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

#[enum_gen_impl(Payload)]
pub fn default(id: usize) -> Payload {
    EnumVariantType(EnumStructType::default())
}

#[enum_gen_impl(Payload)]
pub fn size_of(id: usize) -> usize {
    std::mem::size_of::<EnumStructType>()
}
```

`#[enum_gen(derive(Debug, Default), repr(C, packed))]` is responsible for generating a struct for every enum variant, and attributing each one with `#[derive(Debug, Default)]` and `#[repr(C, packed)]`.

`#[enum_gen_impl(Payload)]` provides EnumVariantType and EnumStructType to be used in the function body, which correspond to enum variant identified by `id`. This could be explained as:

```ignore
# pseudo-syntax
use StructBy<id> as EnumStructType;
use Enum::VariantBy<id> as EnumVariantType;
```

The proc macro actually works by replacing the function body with an `id` match expression, where every match arm is filled with the original body, just preceeded with different `use X as EnumStructType`.

The above full example code is expanded to the following:

```rust
pub enum Payload {
    Hello(Hello),
    Goodbye(Goodbye),
    Invalid(Invalid),
}
#[derive(Debug, Default)]
#[repr(C, packed)]
pub struct Hello {
    pub a: u8,
    pub b: u64,
    pub c: u64,
    pub d: u8,
}
impl Hello {
    pub const ID: usize = 43usize;
}
#[derive(Debug, Default)]
#[repr(C, packed)]
pub struct Goodbye {
    pub a: u8,
    pub e: u8,
}
impl Goodbye {
    pub const ID: usize = 66usize;
}
#[derive(Debug, Default)]
#[repr(C, packed)]
pub struct Invalid {}

pub fn default(id: usize) -> Payload {
    match id {
        43 => {
            use Hello as EnumStructType;
            use Payload::Hello as EnumVariantType;
            EnumVariantType(EnumStructType::default())
        }
        66 => {
            use Goodbye as EnumStructType;
            use Payload::Goodbye as EnumVariantType;
            EnumVariantType(EnumStructType::default())
        }
        _ => {
            use Invalid as EnumStructType;
            use Payload::Invalid as EnumVariantType;
            EnumVariantType(EnumStructType::default())
        }
    }
}

pub fn size_of(id: usize) -> usize {
    match id {
        43 => {
            use Hello as EnumStructType;
            use Payload::Hello as EnumVariantType;
            std::mem::size_of::<EnumStructType>()
        }
        66 => {
            use Goodbye as EnumStructType;
            use Payload::Goodbye as EnumVariantType;
            std::mem::size_of::<EnumStructType>()
        }
        _ => {
            use Invalid as EnumStructType;
            use Payload::Invalid as EnumVariantType;
            std::mem::size_of::<EnumStructType>()
        }
    }
}
```

See the documentation on #[`enum_gen`] and #[`enum_gen_impl`] for additional information.