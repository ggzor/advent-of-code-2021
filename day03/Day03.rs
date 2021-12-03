#![feature(stdin_forwarders)]
use std::env;
use std::io;

fn main() {
    let nums = io::stdin()
        .lines()
        .map(|s| {
            s.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let most_frequent_by_column = |col: usize, skip: Option<&Vec<bool>>| {
        let (b0, b1) = (0..nums.len())
            .filter(|&i| !skip.map_or_else(|| false, |v| v[i]))
            .map(|row| nums[row][col])
            .fold((0, 0), |(b0, b1), b| match b {
                0 => (b0 + 1, b1),
                1 => (b0, b1 + 1),
                _ => (b0, b1),
            });

        if b1 >= b0 {
            1
        } else {
            0
        }
    };

    let most_frequent = |skip: Option<&Vec<bool>>| {
        (0..nums[0].len())
            .map(|col| most_frequent_by_column(col, skip))
            .collect::<Vec<_>>()
    };

    let least_frequent = |v: &Vec<u32>| v.iter().map(|i| 1 - i).collect::<Vec<_>>();
    let as_dec = |v: &Vec<u32>| v.iter().fold(0, |acc, x| acc * 2 + x);

    if env::args().nth(1).unwrap() == "1" {
        let t = most_frequent(None);
        let gamma = as_dec(&t);
        let epsilon = as_dec(&least_frequent(&t));
        println!("{}", gamma * epsilon);
    } else {
        // Part two
        let go = |post: fn(u32) -> u32| {
            let mut skip = vec![false; nums.len()];
            let mut skip_count = 0;

            for col in 0..nums[0].len() {
                if skip_count + 1 == nums.len() {
                    break;
                }

                let bit = post(most_frequent_by_column(col, Some(&skip)));
                for row in 0..nums.len() {
                    if !skip[row] && nums[row][col] != bit {
                        skip_count += 1;
                        skip[row] = true;
                    }
                }
            }

            nums.iter()
                .enumerate()
                .find(|&(i, _)| skip[i] == false)
                .unwrap()
                .1
        };

        let oxygen = as_dec(go(|x| x));
        let co2 = as_dec(go(|x| 1 - x));

        println!("{}", oxygen * co2);
    }
}
