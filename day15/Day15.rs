use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::io;

fn main() {
    type Point = (i32, i32);
    let nums: HashMap<Point, u32> = io::stdin()
        .lines()
        .map(Result::unwrap)
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .map(|(x, c)| ((y as i32, x as i32), c.to_digit(10).unwrap()))
                .collect::<Vec<_>>()
        })
        .collect();

    let bound_by = |f: fn(&Point) -> i32| nums.keys().map(f).max().unwrap() + 1;
    let h = bound_by(|p| p.0);
    let w = bound_by(|p| p.1);

    let (h, w, nums) = if env::args().nth(1).unwrap() == "1" {
        (h, w, nums)
    } else {
        (
            h * 5,
            w * 5,
            nums.iter()
                .flat_map(|((y, x), v)| {
                    (0..5).flat_map(move |sy| {
                        (0..5).map(move |sx| {
                            let nv = match *v + sy as u32 + sx as u32 {
                                x if x <= 9 => x,
                                x => x - 9,
                            };

                            ((*y + sy * h, *x + sx * h), nv)
                        })
                    })
                })
                .collect(),
        )
    };

    let mut dist: HashMap<Point, u32> = HashMap::new();
    let mut q: BinaryHeap<Reverse<(u32, Point)>> = BinaryHeap::new();
    let mut seen: HashSet<Point> = HashSet::new();

    dist.insert((0, 0), 0);
    q.push(Reverse((0, (0, 0))));

    while let Some(Reverse((len, p @ (y, x)))) = q.pop() {
        for (dy, dx) in [(-1, 0), (0, -1), (0, 1), (1, 0)] {
            let np @ (ny, nx) = (y + dy, x + dx);

            if 0 <= ny && ny < h && 0 <= nx && nx < w {
                let &old = dist.get(&np).unwrap_or(&u32::MAX);
                let new = len + nums.get(&np).unwrap();

                if new < old {
                    dist.insert(np, new);
                    q.push(Reverse((new, np)));
                }
            }
        }

        seen.insert(p);
    }

    println!("{}", dist[&(h - 1, w - 1)]);
}
