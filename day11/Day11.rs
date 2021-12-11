#![feature(stdin_forwarders)]
use std::collections::HashSet;
use std::collections::VecDeque;
use std::env;
use std::io;

type Board = Vec<Vec<u32>>;

const NEIGHBORS: [(isize, isize); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn step(board: &mut Board) -> usize {
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();

    for y in 0..board.len() {
        for x in 0..board[0].len() {
            board[y][x] += 1;
            if board[y][x] > 9 {
                queue.push_back((y, x));
            }
        }
    }

    let mut flashed: HashSet<(usize, usize)> = HashSet::new();
    while let Some(item @ (y, x)) = queue.pop_front() {
        if flashed.insert(item) {
            for (dy, dx) in NEIGHBORS {
                // Underflow here, not relevant for the current problem
                // https://glot.io/snippets/g52gjp5zhw
                let np @ (ny, nx) = ((y as isize + dy) as usize, (x as isize + dx) as usize);

                if let Some(v) = board.get_mut(ny).and_then(|r| r.get_mut(nx)) {
                    *v += 1;
                    if *v > 9 && !flashed.contains(&np) {
                        queue.push_back(np);
                    }
                }
            }
        }
    }

    board.iter_mut().flatten().for_each(|v| {
        if *v > 9 {
            *v = 0
        }
    });

    flashed.len()
}

fn main() {
    let mut nums: Board = io::stdin()
        .lines()
        .map(|s| {
            s.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect()
        })
        .collect();

    if env::args().nth(1).unwrap() == "1" {
        println!("{}", (1..=100).map(|_| step(&mut nums)).sum::<usize>());
    } else {
        println!(
            "{}",
            (0..).skip_while(|_| step(&mut nums) != 100).next().unwrap() + 1
        );
    }
}
