# genmatch

Effectively, a lower level [enum_dispatch](https://crates.io/crates/enum_dispatch) crate.

Provides `#[genmatch_self]` and `#[genmatch_id]` macros to automatically generate match expression for given enum's variants. All variants are expected to contain a single unnamed field that is a struct.

`#[genmatch_self]` used on a function provides `inner` alias to the function body, which corresponds to the object stored inside the `self`'s variant.

This works by replacing the function body with a `self` match expression, where every match arm is filled with the original body.

`#[genmatch_id]` works similar, but matches on numerical ID that had to assigned to every enum variant. Then it provides the enum variant (type) and the inner struct (type) to be used inside the function.

This is best explained with an example.

## Example

```rust
use genmatch::{genmatch, genmatch_id, genmatch_self};

#[genmatch]
#[derive(Debug)]
enum Payload {
    #[attr(ID = 0x2b)]
    Hello(Hello),
    #[attr(ID = 0x42)]
    Goodbye(Goodbye),
    #[attr(ID = _)]
    Invalid(Invalid),
}

#[derive(Debug, Default)]
struct Hello {
    pub a: u8,
    pub b: u64,
}

#[derive(Debug, Default)]
struct Goodbye {
    pub e: u8,
}

#[derive(Debug, Default)]
struct Invalid {}

impl Payload {
    #[genmatch_id(Payload)]
    fn default(id: usize) -> Self {
        EnumVariantType(EnumStructType::default())
    }

    #[genmatch_self(Payload)]
    fn size(&self) -> usize {
        std::mem::size_of_val(inner)
    }
}
```

`#[attr(ID = ...)]` are only needed for `#[genmatch_id(...)]`. They can be omitted if only `#[genmatch_self(...)]` is used. An alternative for `#[attr(ID = ...)]` is manually specifying `StructName::ID`, e.g.:

```ignore
impl Hello {
    const ID: usize = 0x42;
}
```

`#[genmatch_id(Payload)]` provides EnumVariantType and EnumStructType to be used in the function body, which correspond to enum variant identified by `id`. This could be explained as:

```ignore
# pseudo-syntax
use StructBy<id> as EnumStructType;
use Enum::VariantBy<id> as EnumVariantType;
```

The proc macro actually works by replacing the function body with an `id` match expression, where every match arm is filled with the original body, just preceeded with different `use X as EnumStructType`. For this reason it's recommended to keep the function body minimal, and put the common logic elsewhere.

The above example code is expanded to the following:

```rust
#[derive(Debug)]
enum Payload {
    Hello(Hello),
    Goodbye(Goodbye),
    Invalid(Invalid),
}

#[derive(Debug, Default)]
struct Hello {
    pub a: u8,
    pub b: u64,
}

#[derive(Debug, Default)]
struct Goodbye {
    pub e: u8,
}

#[derive(Debug, Default)]
struct Invalid {}

impl Payload {
    fn default(id: usize) -> Payload {
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

    fn size(&self) -> usize {
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