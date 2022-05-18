use std::collections::HashMap;
use std::env;
use std::io;
use std::iter::repeat;

type Img = HashMap<(i32, i32), bool>;

fn bools_to_size<T>(bools: T) -> usize
where
    T: IntoIterator<Item = bool>,
{
    bools.into_iter().fold(0, |a, b| a * 2 + b as usize)
}

fn main() {
    let mut lines = io::stdin().lines().map(Result::unwrap);

    let algo: Vec<_> = lines.next().unwrap().chars().map(|c| c == '#').collect();
    let mut img: Img = lines
        .skip(1)
        .enumerate()
        .flat_map(|(y, row)| {
            row.chars()
                .enumerate()
                .map(move |(x, v)| ((y as i32, x as i32), v == '#'))
                .collect::<Vec<_>>()
        })
        .collect();

    let top = if env::args().nth(1).unwrap() == "1" {
        2
    } else {
        50
    };

    for i in 1..=top {
        let empty = if i % 2 == 0 {
            algo[0]
        } else {
            algo[bools_to_size(repeat(algo[0]).take(9))]
        };

        let (mut min_y, mut max_y, mut min_x, mut max_x) = (0, 0, 0, 0);
        for (y, x) in img.keys() {
            min_y = min_y.min(*y);
            max_y = max_y.max(*y);

            min_x = min_x.min(*x);
            max_x = max_x.max(*x);
        }

        let mut new_img: Img = HashMap::new();

        for y in min_y - 1..=max_y + 1 {
            for x in min_x - 1..=max_x + 1 {
                let img = &img; // Move reference, not img
                let idx = bools_to_size((-1..=1).flat_map(|dy| {
                    (-1..=1).map(move |dx| *img.get(&(y + dy, x + dx)).unwrap_or(&empty))
                }));

                new_img.insert((y, x), algo[idx]);
            }
        }

        img = new_img;
    }

    println!("{}", img.values().filter(|x| **x).count());
}
