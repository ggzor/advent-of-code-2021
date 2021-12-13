#![feature(stdin_forwarders)]
use std::collections::HashSet;
use std::env;
use std::io;

fn main() {
    let mut points: HashSet<(u32, u32)> = HashSet::new();
    let mut folds: Vec<(char, u32)> = Vec::new();

    let mut parse_nums = true;
    for l in io::stdin().lines().map(Result::unwrap) {
        if l.is_empty() {
            parse_nums = false;
            continue;
        }

        if parse_nums {
            let (a, b) = l.split_once(',').unwrap();
            points.insert((a.parse().unwrap(), b.parse().unwrap()));
        } else {
            let (a, b) = l.split_once('=').unwrap();
            folds.push((a.chars().last().unwrap(), b.parse().unwrap()));
        }
    }

    let bound_of_axis = |axis: char| {
        folds
            .iter()
            .filter(|(a, _)| *a == axis)
            .map(|(_, v)| v)
            .max()
            .unwrap()
            * 2
            + 1
    };

    let mut w = bound_of_axis('x');
    let mut h = bound_of_axis('y');

    let (top, is_first) = if env::args().nth(1).unwrap() == "1" {
        (1, true)
    } else {
        (folds.len(), false)
    };

    for (dir, n) in folds.iter().take(top) {
        let mut new_points: HashSet<(u32, u32)> = HashSet::new();

        for p @ (x, y) in points.iter() {
            new_points.insert(match dir {
                'x' if x >= n => (w - x - 1, *y),
                'y' if y >= n => (*x, h - y - 1),
                _ => *p,
            });
        }

        match dir {
            'x' => w /= 2,
            'y' => h /= 2,
            _ => (),
        }

        points = new_points;
    }

    if is_first {
        println!("{}", points.len());
    } else {
        for y in 0..h {
            for x in 0..w {
                print!(
                    "{}",
                    match points.contains(&(x, y)) {
                        true => '#',
                        false => ' ',
                    }
                )
            }
            println!();
        }
    }
}
