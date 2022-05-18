#![feature(array_windows)]
use std::collections::HashMap;
use std::env;
use std::io;

fn main() {
    let mut ls = io::stdin().lines().map(Result::unwrap);
    let template = ls.next().unwrap();
    ls.next();

    let rules: HashMap<(char, char), char> = ls.fold(HashMap::new(), |mut r, s| {
        let (a, b) = s.split_once(" -> ").unwrap();
        let [r1, r2] = [0, 1].map(|i| a.chars().nth(i).unwrap());
        r.insert((r1, r2), b.chars().nth(0).unwrap());
        r
    });

    let mut char_counts: HashMap<char, u64> = template.chars().fold(HashMap::new(), |mut cs, c| {
        *cs.entry(c).or_default() += 1;
        cs
    });

    let mut pairs: HashMap<(char, char), u64> = template
        .chars()
        .collect::<Vec<_>>()
        .array_windows()
        .fold(HashMap::new(), |mut ps, [c1, c2]| {
            *ps.entry((*c1, *c2)).or_default() += 1;
            ps
        });

    let top: u64 = [10, 40][env::args().nth(1).unwrap().parse::<usize>().unwrap() - 1];

    for _ in 1..=top {
        let mut new_pairs: HashMap<(char, char), u64> = HashMap::new();

        for (p @ (p1, p2), v) in pairs {
            if let Some(&mid) = rules.get(&p) {
                *new_pairs.entry((p1, mid)).or_default() += v;
                *new_pairs.entry((mid, p2)).or_default() += v;
                *char_counts.entry(mid).or_default() += v;
            }
        }

        pairs = new_pairs;
    }

    println!(
        "{}",
        char_counts.values().max().unwrap() - char_counts.values().min().unwrap()
    );
}
