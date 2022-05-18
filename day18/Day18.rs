#![feature(if_let_guard, array_windows)]
use std::collections::HashMap;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::env;
use std::io;

// Maybe indexmap would work better here?
// Or a BTreeMap with a clever keying scheme?
// Fortunately the trees are not that big, so it works fine.
type Path = VecDeque<char>;
type Tree = VecDeque<(u32, Path)>;

fn parse_tree<T>(s: T) -> Tree
where
    T: AsRef<str>,
{
    s.as_ref()
        .chars()
        .fold(
            (VecDeque::new(), VecDeque::new()),
            |(mut result, mut path), c| {
                match c {
                    '[' => { path.push_back('L'); }
                    ',' => { path.pop_back(); path.push_back('R'); }
                    ']' => { path.pop_back(); }
                    // No multi-digit numbers in my input set
                    c if let Some(d) = c.to_digit(10)  => {
                        result.push_back((d, path.clone()));
                    }
                    _ => unreachable!(),
                }
                (result, path)
            },
        )
        .0
}

fn try_explode(t: &mut Tree) -> Result<(), ()> {
    let mut to_explode = None;
    for (i, (_, ap)) in t.iter().rev().skip(1).rev().enumerate() {
        let (_, bp) = t.get(i + 1).unwrap();

        if ap.len() == 5 && ap.iter().take(4).eq(bp.iter().take(4)) {
            to_explode = Some(i);
            break;
        }
    }

    if let Some(i) = to_explode {
        let (av, mut ap) = t.remove(i).unwrap();
        let (bv, _) = t.remove(i).unwrap();

        let left = (i as isize - 1).try_into().ok().and_then(|i| t.get_mut(i));
        if let Some((v, _)) = left {
            *v += av;
        }

        let right = t.get_mut(i);
        if let Some((v, _)) = right {
            *v += bv;
        }

        ap.pop_back();
        t.insert(i, (0, ap));

        Ok(())
    } else {
        Err(())
    }
}

fn try_split(t: &mut Tree) -> Result<(), ()> {
    let mut to_split = None;

    for (i, (v, _)) in t.iter().enumerate() {
        if *v >= 10 {
            to_split = Some(i);
            break;
        }
    }

    if let Some(i) = to_split {
        let (av, mut ap) = t.remove(i).unwrap();

        let mut lap = ap.clone();
        lap.push_back('L');
        t.insert(i, (av / 2, lap));

        ap.push_back('R');
        t.insert(i + 1, (av - av / 2, ap));

        Ok(())
    } else {
        Err(())
    }
}

fn go_magnitude(path: &mut Path, idx: &HashMap<&Path, u32>) -> u32 {
    if let Some(v) = idx.get(path) {
        return *v;
    } else {
        path.push_back('L');
        let l = go_magnitude(path, idx);
        path.pop_back();

        path.push_back('R');
        let r = go_magnitude(path, idx);
        path.pop_back();

        return 3 * l + 2 * r;
    }
}

fn add_trees(mut l: &mut Tree, r: &Tree) {
    l.iter_mut().for_each(|(_, p)| p.push_front('L'));
    let mut r = r.clone();
    r.iter_mut().for_each(|(_, p)| p.push_front('R'));
    l.extend(r);

    while try_explode(&mut l).is_ok() || try_split(&mut l).is_ok() {}
}

fn magnitude(t: &Tree) -> u32 {
    let idx: HashMap<&Path, u32> = t.iter().map(|(v, p)| (p, *v)).collect();
    return go_magnitude(&mut VecDeque::new(), &idx);
}

fn main() {
    let trees = io::stdin().lines().map(Result::unwrap).map(parse_tree);

    if env::args().nth(1).unwrap() == "1" {
        let result = trees
            .into_iter()
            .reduce(|mut l, r| {
                add_trees(&mut l, &r);

                while try_explode(&mut l).is_ok() || try_split(&mut l).is_ok() {}

                l
            })
            .unwrap();

        println!("{}", magnitude(&result));
    } else {
        let mut max = 0;
        let trees: Vec<_> = trees.collect();

        for i in 0..trees.len() {
            for j in i + 1..trees.len() {
                for (a, b) in [(i, j), (j, i)] {
                    let mut first = trees[a].clone();
                    add_trees(&mut first, &trees[b]);

                    max = max.max(magnitude(&first));
                }
            }
        }

        println!("{}", max);
    }
}
