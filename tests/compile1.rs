/* SPDX-License-Identifier: MIT
 * Copyright(c) 2023 Darek Stojaczyk
 */

use zerocopy_enum::*;

#[derive(ZerocopyEnum)]
pub enum Payload {
    Hello { a: u8, b: u64, c: u64, d: u8 },
    Unknown,
}

#[test]
fn struct_init() {
    let _ = Hello {
        a: 127,
        b: 0,
        c: 0,
        d: 1,
    };
}
