#![feature(stdin_forwarders)]
use std::env;
use std::io;

fn main() {
    let stmts = io::stdin()
        .lines()
        .map(|s| {
            let s = s.ok()?;
            let mut w = s.split(" ");

            let dir = w.next()?.to_owned();
            let n = w.next()?.parse::<u32>().ok()?;

            Some((dir, n))
        })
        .map(Option::unwrap);

    let (mut x, mut y, mut aim) = (0, 0, 0);

    if env::args().nth(1).unwrap() == "1" {
        for inst in stmts {
            match (inst.0.as_str(), inst.1) {
                ("forward", n) => x += n,
                ("down", n) => y += n,
                ("up", n) => y -= n,
                _ => (),
            }
        }
    } else {
        // Part two
        for inst in stmts {
            match (inst.0.as_str(), inst.1) {
                ("down", n) => aim += n,
                ("up", n) => aim -= n,
                ("forward", n) => {
                    x += n;
                    y += aim * n;
                }
                _ => (),
            }
        }
    }

    println!("{}", x * y);
}
