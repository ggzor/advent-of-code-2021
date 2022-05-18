use std::collections::HashMap;
use std::env;
use std::io;

fn main() {
    let nums: Vec<Vec<u32>> = io::stdin()
        .lines()
        .map(|s| s.unwrap())
        .map(|s| {
            s.split(" -> ")
                .flat_map(|s| s.split(","))
                .map(str::parse)
                .map(Result::unwrap)
                .collect()
        })
        .collect();

    let step = |a: u32, b: u32| {
        if a < b {
            a + 1
        } else if a > b {
            a - 1
        } else {
            a
        }
    };

    let criteria: fn(&&Vec<u32>) -> bool = if env::args().nth(1).unwrap() == "1" {
        // Part one, use only horizontal and vertical
        |&v: &&Vec<u32>| match v[..] {
            [x1, y1, x2, y2] => x1 == x2 || y1 == y2,
            _ => false,
        }
    } else {
        // Part two, use all
        |_| true
    };

    let mut board: HashMap<(u32, u32), u32> = HashMap::new();

    for v in nums.iter().filter(criteria) {
        match v[..] {
            [x1, y1, x2, y2] => {
                let mut x = x1;
                let mut y = y1;

                while !(x == x2 && y == y2) {
                    *board.entry((y, x)).or_insert(0) += 1;

                    x = step(x, x2);
                    y = step(y, y2);
                }

                *board.entry((y, x)).or_insert(0) += 1;
            }
            _ => (),
        }
    }

    let count = board.iter().map(|kv| kv.1).filter(|&&v| v > 1).count();

    println!("{}", count);
}
