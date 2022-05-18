use std::collections::HashMap;
use std::env;
use std::io;

fn main() {
    let nums: Vec<u64> = io::stdin()
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(",")
        .map(str::parse)
        .map(Result::unwrap)
        .collect();

    let mut ls: HashMap<u64, u64> = Default::default();
    for n in nums {
        *ls.entry(n).or_default() += 1;
    }

    let top = if env::args().nth(1).unwrap() == "1" {
        80
    } else {
        256
    };

    for _ in 1..=top {
        let mut new: HashMap<u64, u64> = Default::default();
        for (k, v) in &ls {
            if *k == 0 {
                *new.entry(6).or_default() += v;
                *new.entry(8).or_default() += v;
            } else {
                *new.entry(k - 1).or_default() += v;
            }
        }
        ls = new;
    }

    println!("{}", ls.values().sum::<u64>());
}
