#![feature(stdin_forwarders)]
use std::collections::HashMap;
use std::io;

fn main() {
    let mut cucumbers: HashMap<(u8, u8), char> = HashMap::new();

    let mut w = 0;
    let mut h = 0;

    for (y, row) in io::stdin().lines().enumerate() {
        for (x, c) in row.unwrap().chars().enumerate() {
            if matches!(c, '>' | 'v') {
                cucumbers.insert((y as u8, x as u8), c);
            }

            w = w.max(x as u8);
            h = h.max(y as u8);
        }
    }

    w += 1;
    h += 1;

    let mut steps = 0;
    loop {
        let mut changed = 0;

        let mut new_cucumbers = HashMap::new();
        for (&(y, x), &c) in cucumbers.iter() {
            if c == '>' && !cucumbers.contains_key(&(y, (x + 1) % w)) {
                changed += 1;
                new_cucumbers.insert((y, (x + 1) % w), c);
            } else {
                new_cucumbers.insert((y, x), c);
            }
        }
        cucumbers = new_cucumbers;

        let mut new_cucumbers = HashMap::new();

        for (&(y, x), &c) in cucumbers.iter() {
            if c == 'v' && !cucumbers.contains_key(&((y + 1) % h, x)) {
                changed += 1;
                new_cucumbers.insert(((y + 1) % h, x), c);
            } else {
                new_cucumbers.insert((y, x), c);
            }
        }

        cucumbers = new_cucumbers;

        if changed == 0 {
            break;
        }

        steps += 1;
    }

    println!("{}", steps + 1);
}
