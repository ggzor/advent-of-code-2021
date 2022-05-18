#![feature(type_alias_impl_trait)]
use std::collections::HashMap;
use std::env;
use std::io;
use std::iter::repeat;

fn main() {
    let mut nums = io::stdin().lines().map(|s| {
        s.unwrap()
            .split_whitespace()
            .last()
            .unwrap()
            .parse()
            .unwrap()
    });

    let is_first = env::args().nth(1).unwrap() == "1";

    let dice100: Vec<_> = (1..=100).collect();
    let dice100 = dice100.iter().cycle().map(|i| [*i, *i, *i]);
    let dice_dirac = repeat([1, 2, 3]);

    let mut dice: Box<dyn Iterator<Item = [i32; 3]>> = if is_first {
        Box::new(dice100)
    } else {
        Box::new(dice_dirac)
    };

    let mut universes: HashMap<([i32; 2], [i32; 2]), u64> = HashMap::new();
    universes.insert(([nums.next().unwrap(), nums.next().unwrap()], [0, 0]), 1);

    let mut roll_count = 0;
    let mut turn = 0;

    let mut counts = [0, 0];
    let mut last_scores = [0, 0];

    let top = if is_first { 1000 } else { 21 };

    while !universes.is_empty() {
        let r1 = dice.next().unwrap();
        let r2 = dice.next().unwrap();
        let r3 = dice.next().unwrap();

        // No easy way around Sized
        let nitems = if is_first { 1 } else { 3 };

        let mut new_universes = HashMap::new();

        for &r1 in r1.iter().take(nitems) {
            for &r2 in r2.iter().take(nitems) {
                for &r3 in r3.iter().take(nitems) {
                    for ((positions, scores), c) in universes.iter() {
                        let total = r1 + r2 + r3;

                        let mut positions = positions.clone();
                        let mut scores = scores.clone();

                        positions[turn] = ((positions[turn] - 1 + total) % 10) + 1;
                        scores[turn] += positions[turn];

                        if scores.iter().max().unwrap() >= &top {
                            if scores[0] > scores[1] {
                                counts[0] += c;
                            } else {
                                counts[1] += c;
                            }

                            last_scores = scores;
                        } else {
                            *new_universes.entry((positions, scores)).or_default() += *c;
                        }
                    }
                }
            }
        }

        turn = 1 - turn;
        roll_count += 3;

        universes = new_universes;
    }

    if is_first {
        println!("{}", roll_count * last_scores.iter().min().unwrap());
    } else {
        println!("{}", counts.iter().max().unwrap());
    }
}
