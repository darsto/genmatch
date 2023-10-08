# enum_gen

Provides the `#[enum_gen]` procedural macro to generate structures from enum variants.
The additional `#[enum_gen_match_self]` and `#[enum_gen_match_id]` macros allow automatically generating a match expression for the variants.

The `#[enum_gen_match_self]` macro can be essentially used to manually implement [#[enum_dispatch]](https://crates.io/crates/enum_dispatch), but with higher versability.

The `#[enum_gen_match_id]` macro can be used to match an enum variant by numerical ID, which has to be assigned to the variant in the orignal enum definition.

This is best explained with an example.

## Example

```rust
use enum_gen::{enum_gen, enum_gen_match_id, enum_gen_match_self};

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
}

#[enum_gen_match_id(Payload)]
pub fn size_of_payload(id: usize) -> usize {
    std::mem::size_of::<EnumStructType>()
}

impl Payload {
    #[enum_gen_match_self(Payload)]
    pub fn size(&self) -> usize {
        std::mem::size_of_val(inner)
    }
}
```

`#[enum_gen(derive(Debug, Default), repr(C, packed))]` is responsible for generating a struct for every enum variant, and attributing each one with `#[derive(Debug, Default)]` and `#[repr(C, packed)]`.

`#[enum_gen_match_id(Payload)]` provides EnumVariantType and EnumStructType to be used in the function body, which correspond to enum variant identified by `id`. This could be explained as:

```ignore
# pseudo-syntax
use StructBy<id> as EnumStructType;
use Enum::VariantBy<id> as EnumVariantType;
```

The proc macro actually works by replacing the function body with an `id` match expression, where every match arm is filled with the original body, just preceeded with different `use X as EnumStructType`. For this reason it's recommended to keep the function body minimal, potentially separating the generic logic to another helper function: `fn inner_logic_not_worth_duplicating<T: MyTrait>(v: &T)`.

Lastly, `#[enum_gen_match_self(Payload)]` works the same as `#[enum_gen_match_id(Payload)]`, but matches on `self` instead. The inner structure of variant is available through `inner` variable. This macro is applicable to functions with either `self`, `&self`, or `&mut self` parameter.

The above example code is expanded to the following:

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

impl Payload {
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
}

pub fn size_of_payload(id: usize) -> usize {
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

impl Payload {
    pub fn size(&self) -> usize {
        match self {
            Payload::Hello(inner) => {
                use Hello as EnumStructType;
                use Payload::Hello as EnumVariantType;
                std::mem::size_of_val(inner)
            }
            Payload::Goodbye(inner) => {
                use Goodbye as EnumStructType;
                use Payload::Goodbye as EnumVariantType;
                std::mem::size_of_val(inner)
            }
            Payload::Invalid(inner) => {
                use Invalid as EnumStructType;
                use Payload::Invalid as EnumVariantType;
                std::mem::size_of_val(inner)
            }
        }
    }
}
```

See the macros documentation for additional information.