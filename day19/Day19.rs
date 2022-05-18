#![feature(array_zip)]
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::env;
use std::io;

const PERMUTATIONS: [[i32; 3]; 24] = [
    [1, 2, 3],
    [-3, 2, 1],
    [-1, 2, -3],
    [3, 2, -1],
    [-1, -2, 3],
    [-3, -2, -1],
    [1, -2, -3],
    [3, -2, 1],
    [-2, 1, 3],
    [-2, -3, 1],
    [-2, -1, -3],
    [-2, 3, -1],
    [2, -1, 3],
    [2, -3, -1],
    [2, 1, -3],
    [2, 3, 1],
    [1, -3, 2],
    [-3, -1, 2],
    [-1, 3, 2],
    [3, 1, 2],
    [1, 3, -2],
    [-3, 1, -2],
    [-1, -3, -2],
    [3, -1, -2],
];

type V3 = [i32; 3];

fn main() {
    let mut scanners: Vec<Vec<V3>> =
        io::stdin()
            .lines()
            .map(Result::unwrap)
            .fold(Vec::new(), |mut res, line| {
                if line.starts_with("--") {
                    res.push(Vec::new());
                } else if !line.is_empty() {
                    let last = res.last_mut().unwrap();
                    let mut new = [0; 3];
                    line.split(',')
                        .map(|s| s.parse().unwrap())
                        .enumerate()
                        .for_each(|(i, v)| new[i] = v);
                    last.push(new);
                }

                res
            });

    let mut distances: HashMap<usize, V3> = HashMap::new();
    distances.insert(0, [0, 0, 0]);

    let mut next: VecDeque<usize> = VecDeque::new();
    next.push_back(0);

    let mut remaining: HashSet<usize> = HashSet::new();
    remaining.extend(1..scanners.len());

    while let Some(a) = next.pop_front() {
        for b in remaining.clone() {
            let ascan = &scanners[a];
            let bscan = &scanners[b];

            'outer: for p in &PERMUTATIONS {
                let idx = p.map(|i| (i.unsigned_abs() - 1) as usize);
                let sign = p.map(|i| i.signum());
                let mapper = idx.zip(sign);

                let nscan: Vec<_> = bscan
                    .iter()
                    .map(|v| mapper.map(|(i, s)| v[i] * s))
                    .collect();

                for av in ascan.iter() {
                    for nv in nscan.iter() {
                        let diff = nv.zip(*av).map(|(x, y)| x - y);

                        let temp: Vec<V3> = nscan
                            .iter()
                            .map(|v| v.zip(diff).map(|(x, y)| x - y))
                            .collect();

                        let count = ascan
                            .iter()
                            .flat_map(|v1| temp.iter().filter(move |&v2| v1 == v2))
                            .count();

                        if count >= 12 {
                            scanners[b] = temp;
                            distances.insert(b, diff);
                            next.push_back(b);
                            remaining.remove(&b);
                            break 'outer;
                        }
                    }
                }
            }
        }
    }

    if env::args().nth(1).unwrap() == "1" {
        println!(
            "{}",
            scanners.iter().flatten().collect::<HashSet<_>>().len()
        );
    } else {
        let mut max = 0;

        for i in 0..scanners.len() {
            for j in i + 1..scanners.len() {
                let manhattan = distances
                    .get(&i)
                    .and_then(|v1| {
                        distances.get(&j).map(|v2| {
                            v1.zip(*v2) //
                                .map(|(x, y)| (x - y).abs())
                                .iter()
                                .sum()
                        })
                    })
                    .unwrap_or_default();
                max = max.max(manhattan);
            }
        }

        println!("{}", max);
    }
}
