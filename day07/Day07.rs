use std::convert::identity;
use std::env;
use std::io;

fn main() {
    let nums: Vec<i64> = io::stdin()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(",")
        .map(|s| s.parse().unwrap())
        .collect();
    let top = *nums.iter().max().unwrap();

    let f: fn(i64) -> i64 = if env::args().nth(1).unwrap() == "1" {
        identity
    } else {
        |x| x * (x + 1) / 2
    };

    let result: i64 = (0..=top)
        .map(|i| nums.iter().map(|j| f((i - j).abs())).sum())
        .min()
        .unwrap();

    println!("{}", result);
}
