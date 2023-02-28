use std::collections::HashMap;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

static ROMAN_MAP: [(usize, &str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

fn to_string1(n: usize) -> String {
    let mut start = n;
    let mut result = String::new();
    for &(value, symbol) in ROMAN_MAP.iter() {
        while start >= value {
            result.push_str(symbol);
            start -= value;
        }
    }
    result
}

fn to_string2(n: usize) -> String {
    let mut start = n;
    let lookup = HashMap::from(ROMAN_MAP);
    let mut result = String::new();
    while start > 0 {
        let max_val = *lookup.keys().filter(|&x| x <= &start).max().unwrap();
        result.push_str(lookup[&max_val]);
        start -= max_val;
    }
    result
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("to_string1", |b| b.iter(|| to_string1(black_box(93))));
    c.bench_function("to_string2", |b| b.iter(|| to_string2(black_box(93))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
