use std::collections::{hash_map::DefaultHasher, BinaryHeap, HashMap, HashSet};
use std::env;
use std::hash::{Hash, Hasher};
use std::io;

type Board = HashMap<(u8, u8), char>;
type BoardKey = u64;

#[derive(PartialEq, Eq, Clone)]
struct BoardItem {
    cost: u32,
    board: Board,
}

impl Ord for BoardItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for BoardItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.cost.partial_cmp(&self.cost)
    }
}

fn board_key(h: u8, w: u8, b: &Board) -> BoardKey {
    let mut hash = DefaultHasher::new();

    // Order is relevant
    for y in 0..h {
        for x in 0..w {
            b.get(&(y, x)).unwrap_or(&'.').hash(&mut hash)
        }
    }

    hash.finish()
}

fn get_target(c: char) -> u8 {
    match c {
        'A' => 3,
        'B' => 5,
        'C' => 7,
        'D' => 9,
        _ => 0,
    }
}

fn cost_for(c: char) -> u32 {
    match c {
        'A' => 1,
        'B' => 10,
        'C' => 100,
        'D' => 1000,
        _ => 0,
    }
}

fn is_target(x: u8) -> bool {
    matches!(x, 3 | 5 | 7 | 9)
}

fn one_side<'a>(
    h: u8,
    _w: u8,
    board: &'a Board,
    src: (u8, u8),
    r: impl Iterator<Item = u8> + 'a,
) -> impl Iterator<Item = (u8, u8)> + 'a {
    let vs = board.get(&src).unwrap(); // Must not fail

    r.map(move |xi| ((1, xi), *board.get(&(1, xi)).unwrap_or(&'.')))
        .take_while(|(_, v)| *v == '.')
        .flat_map(move |((yi, xi), _)| {
            let mut last: Option<(u8, u8)> = None;

            if xi == get_target(*vs) {
                let mut room = (2..h - 1)
                    .map(|yr| ((yr, xi), *board.get(&(yr, xi)).unwrap_or(&'.')))
                    .peekable();

                // Poor man's Data.List.span
                while let Some((p, vr)) = room.peek() {
                    if *vr == '.' {
                        last = Some(*p);
                        room.next();
                    } else {
                        break;
                    }
                }

                if room.all(|(_, vr)| vr == *vs) {
                } else {
                    last = None;
                }
            }

            let when_not_target = if !is_target(xi) && last.is_none() {
                Some((yi, xi))
            } else {
                None
            };

            when_not_target.into_iter().chain(last)
        })
}

fn apply_movement(board: &Board, from: (u8, u8), to: (u8, u8)) -> Board {
    let mut result = board.clone();
    result.remove(&from);
    result.insert(to, *board.get(&from).unwrap()); // Must not fail
    result
}

fn distance((y1, x1): (u8, u8), (y2, x2): (u8, u8)) -> u32 {
    x1.abs_diff(x2) as u32 + (y1 - 1) as u32 + (y2 - 1) as u32
}

fn main() {
    let board: Board = io::stdin()
        .lines()
        .map(Result::unwrap)
        .enumerate()
        .flat_map(|(y, s)| {
            s.chars()
                .enumerate()
                .filter(|(_, c)| matches!(c, 'A' | 'B' | 'C' | 'D'))
                .map(|(x, c)| ((y as u8, x as u8), c))
                .collect::<Vec<_>>()
        })
        .collect();

    let h: u8 = if env::args().nth(1).unwrap() == "1" {
        5
    } else {
        7
    };

    let w: u8 = 13;

    let mut q: BinaryHeap<BoardItem> = BinaryHeap::new();
    q.push(BoardItem {
        cost: 0,
        board: board.clone(),
    });

    let mut seen: HashSet<BoardKey> = HashSet::new();
    let mut best = None;

    while let Some(BoardItem { cost, board }) = q.pop() {
        if board
            .iter()
            .all(|((y, x), v)| *y >= 2 && *x == get_target(*v))
        {
            best = Some(cost);
            break;
        }

        if !seen.contains(&board_key(h, w, &board)) {
            seen.insert(board_key(h, w, &board));

            for (&src @ (y, x), &v) in &board {
                let left = one_side(h, w, &board, src, (1..=x - 1).rev());
                let right = one_side(h, w, &board, src, x + 1..w - 1);
                let both_sides = left.chain(right);

                let push_target = |target: (u8, u8)| {
                    let new_board = apply_movement(&board, src, target);

                    if !seen.contains(&board_key(h, w, &new_board)) {
                        q.push(BoardItem {
                            cost: cost + distance(src, target) * cost_for(v),
                            board: new_board,
                        })
                    }
                };

                if y >= 2
                    && (x != get_target(v)
                        || (y + 1..h - 1).any(|yi| board.get(&(yi, x)) != Some(&v)))
                    && (2..=y - 1).all(|yi| !board.contains_key(&(yi, x)))
                {
                    both_sides.for_each(push_target);
                } else if y == 1 {
                    both_sides.filter(|(yi, _)| *yi != 1).for_each(push_target);
                }
            }
        }
    }

    println!("{}", best.unwrap_or(0));
}
