pub type Index = u16;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Range {
    pub start: Index,
    pub end: Index,
}

impl Range {
    pub fn default() -> Range {
        Range { start: 0, end: 0 }
    }

    pub fn new(start: u32, end: u32) -> Range {
        Range {
            start: start as Index,
            end: end as Index,
        }
    }
}

pub fn is_digit(c: u8) -> bool {
    c >= b'0' && c <= b'9'
}

pub fn is_alpha(c: u8) -> bool {
    (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_'
}

pub fn is_ascci(c: u8) -> bool {
    (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || (c >= b'0' && c <= b'9') || c == b'_'
}

pub fn parse_string(string: &[u8]) -> usize {
    let mut number = 0;
    let mut multiply = 1;
    let len = string.len();

    for i in 0..len {
        let c = string[len - i - 1] - b'0';

        number += c as usize * multiply;
        multiply *= 10;
    }

    number
}

pub fn parse_number(number: u32, buffer: &mut [u8]) -> &[u8] {
    let mut string: [u8; 20] = [0; 20];
    let mut length = 0;
    let mut num = number;

    if num == 0 {
        string[0] = b'0';
        length += 1;
    }

    while num > 0 {
        let rem = num as u8 % 10;

        string[length] = rem + b'0';
        length += 1;
        num /= 10;
    }

    for k in 0..length {
        buffer[k] = string[length - k - 1];
    }

    &buffer[0..length]
}

pub fn hash(string: &[u8]) -> u32 {
    let mut h: u32 = 0;

    for c in string {
        h += *c as u32;
    }

    h
}

pub fn compare(first: &[u8], second: &[u8]) -> bool {
    let len = first.len();
    if len != second.len() {
        return false;
    }

    for i in 0..len as usize {
        if first[i] != second[i] {
            return false;
        }
    }

    true
}
