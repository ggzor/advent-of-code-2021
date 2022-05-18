#![feature(type_alias_impl_trait)]
use std::env;
use std::io;
use std::iter::from_fn;
use std::ops;
use std::str::FromStr;

#[derive(Clone, Copy, Debug)]
struct Range {
    start: i64,
    end: i64,
}

impl Range {
    fn new(start: i64, end: i64) -> Option<Self> {
        if start <= end {
            Some(Range { start, end })
        } else {
            None
        }
    }

    fn count(&self) -> usize {
        (self.end - self.start) as usize + 1
    }
}

impl ops::BitAnd for Range {
    type Output = Option<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let (r1, r2) = if self.end > rhs.end {
            (rhs, self)
        } else {
            (self, rhs)
        };

        if r1.end < r2.start {
            None
        } else if r1.start >= r2.start {
            Some(r1)
        } else {
            Some(Range {
                start: r1.start.max(r2.start),
                end: r1.end.min(r2.end),
            })
        }
    }
}

impl FromStr for Range {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end) = s.split_once("..").ok_or(())?;
        Range::new(start.parse().map_err(|_| ())?, end.parse().map_err(|_| ())?).ok_or(())
    }
}

#[derive(Clone, Copy, Debug)]
struct Cube {
    xs: Range,
    ys: Range,
    zs: Range,
}

impl Cube {
    fn new(xs: Range, ys: Range, zs: Range) -> Self {
        Self { xs, ys, zs }
    }

    fn count(&self) -> usize {
        self.xs.count() * self.ys.count() * self.zs.count()
    }
}

impl ops::BitAnd for Cube {
    type Output = Option<Cube>;

    fn bitand(self, rhs: Self) -> Self::Output {
        Some(Cube::new(
            (self.xs & rhs.xs)?,
            (self.ys & rhs.ys)?,
            (self.zs & rhs.zs)?,
        ))
    }
}

fn generate_cube_for(count: i32, lhs: Cube, rhs: Cube) -> Option<Cube> {
    match count {
        1 => Some(Cube::new(
            Range::new(lhs.xs.start, rhs.xs.start - 1)?,
            (lhs.ys & rhs.ys)?,
            (lhs.zs & rhs.zs)?,
        )),
        2 => Some(Cube::new(
            Range::new(rhs.xs.end + 1, lhs.xs.end)?,
            (lhs.ys & rhs.ys)?,
            (lhs.zs & rhs.zs)?,
        )),
        3 => Some(Cube::new(
            lhs.xs,
            Range::new(lhs.ys.start, rhs.ys.start - 1)?,
            (lhs.zs & rhs.zs)?,
        )),
        4 => Some(Cube::new(
            lhs.xs,
            Range::new(rhs.ys.end + 1, lhs.ys.end)?,
            (lhs.zs & rhs.zs)?,
        )),
        5 => Some(Cube::new(
            lhs.xs,
            lhs.ys,
            Range::new(lhs.zs.start, rhs.zs.start - 1)?,
        )),
        6 => Some(Cube::new(
            lhs.xs,
            lhs.ys,
            Range::new(rhs.zs.end + 1, lhs.zs.end)?,
        )),
        _ => None,
    }
}

impl ops::Sub for Cube {
    type Output = impl Iterator<Item = Cube>;

    fn sub(self, rhs: Self) -> Self::Output {
        let intersects = (self & rhs).is_some();

        // Poor man's generator
        let mut count = 0;
        from_fn(move || {
            if !intersects {
                count += 1;
                match count {
                    1 => Some(self),
                    _ => None,
                }
            } else {
                loop {
                    count += 1;
                    let result = generate_cube_for(count, self, rhs);

                    if result.is_some() {
                        return result;
                    } else if count > 6 {
                        return None;
                    }
                }
            }
        })
    }
}

fn main() {
    let mut cubes: Vec<Cube> = Vec::new();
    let first_part_range = Range {
        start: -50,
        end: 50,
    };
    let first_part_cube = Cube::new(first_part_range, first_part_range, first_part_range);

    for l in io::stdin().lines().map(Result::unwrap) {
        let (action, rest) = l.split_once(' ').unwrap();
        let parts: Vec<Range> = rest
            .split(',')
            .map(|s| (&s[2..].parse()).unwrap())
            .collect();
        let new_one = Cube::new(parts[0], parts[1], parts[2]);
        let new_one = if env::args().nth(1).unwrap() == "1" {
            new_one & first_part_cube
        } else {
            Some(new_one)
        };

        if let Some(new_one) = new_one {
            match action {
                "on" => {
                    let mut to_add = vec![new_one];

                    for c in &cubes {
                        let mut new_to_add = Vec::new();

                        for ca in to_add {
                            new_to_add.extend(ca - *c);
                        }

                        to_add = new_to_add;
                    }

                    cubes.extend(to_add);
                }
                "off" => {
                    let mut new_cubes = Vec::new();

                    for c in cubes {
                        new_cubes.extend(c - new_one);
                    }

                    cubes = new_cubes;
                }
                _ => unreachable!(),
            }
        }
    }

    println!("{}", cubes.iter().map(Cube::count).sum::<usize>());
}
