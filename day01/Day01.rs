#![feature(array_windows)]
use std::env;
use std::io;

fn main() {
    let mut nums = io::stdin()
        .lines()
        .map(|s| s.unwrap().parse::<u32>().unwrap())
        .collect::<Vec<_>>(); // 1000 elements won't hurt

    if env::args().nth(1).unwrap() == "2" {
        // Part two
        nums = nums
            .array_windows::<3>()
            .map(|&w| w.iter().sum())
            .collect::<Vec<_>>();
    }

    let result = nums
        .array_windows::<2>() //
        .filter(|&w| w[0] < w[1])
        .count();

    println!("{}", result);
}
