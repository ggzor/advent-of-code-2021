#![feature(stdin_forwarders)]
use std::collections::HashSet;
use std::collections::VecDeque;
use std::env;
use std::io;

fn main() {
    let nums: Vec<Vec<u32>> = io::stdin()
        .lines()
        .map(|s| {
            s.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect()
        })
        .collect();

    let h = nums.len();
    let w = nums[0].len();

    let neighbors = |y: usize, x: usize| {
        (-1..=1).flat_map(move |dy| {
            (-1..=1).filter_map(move |dx| {
                if !(dy == 0 && dx == 0) {
                    let ny = (y as i32) + dy;
                    let nx = (x as i32) + dx;

                    if 0 <= ny && ny < (h as i32) && 0 <= nx && nx < (w as i32) {
                        return Some((ny as usize, nx as usize));
                    }
                }
                None
            })
        })
    };

    let mut lower_points: Vec<(usize, usize)> = Vec::new();
    for y in 0..h {
        for x in 0..w {
            let value = nums[y][x];
            if neighbors(y, x)
                .map(|(y, x)| nums[y][x])
                .all(|nv| value < nv)
            {
                lower_points.push((y, x));
            }
        }
    }

    if env::args().nth(1).unwrap() == "1" {
        let result: u32 = lower_points
            .iter()
            .map(|(y, x)| nums[*y][*x])
            .map(|v| v + 1)
            .sum();

        println!("{:?}", result);
    } else {
        let mut basin_sizes: Vec<usize> = Vec::new();

        for initial in lower_points.iter() {
            let mut seen: HashSet<(usize, usize)> = HashSet::new();
            let mut queue = VecDeque::from([*initial]);

            while let Some(next @ (y, x)) = queue.pop_front() {
                if !seen.contains(&next) {
                    let value = nums[y][x];
                    for p @ (ny, nx) in neighbors(y, x) {
                        if y == ny || x == nx {
                            let nv = nums[ny][nx];

                            if nv != 9 && nv > value {
                                queue.push_back(p);
                            }
                        }
                    }

                    seen.insert(next);
                }
            }

            basin_sizes.push(seen.len());
        }

        basin_sizes.sort_unstable();
        basin_sizes.reverse();

        let result: usize = basin_sizes.iter().take(3).product();
        println!("{}", result);
    }
}
