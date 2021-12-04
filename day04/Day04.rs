#![feature(stdin_forwarders)]
use std::collections::HashSet;
use std::env;
use std::io;

const BOARD_SIZE: usize = 5;

type Board = Vec<Vec<(usize, bool)>>;
type RevIdx = Vec<Option<(usize, usize)>>;

fn parse_boards() -> (Vec<Board>, Vec<RevIdx>) {
    let xs = io::stdin()
        .lines() //
        .filter_map(Result::ok)
        .flat_map(|s| {
            s.split(" ")
                .filter_map(|s| s.parse().ok())
                .collect::<Vec<usize>>()
        });

    let mut boards = vec![];
    let mut rev_indexes = vec![];

    {
        let mut b = vec![vec![(0, false); BOARD_SIZE]; BOARD_SIZE];
        let mut idx = vec![];

        let mut row = 0;
        let mut col = 0;

        for x in xs {
            b[row][col] = (x, false);

            if x + 1 > idx.len() {
                idx.resize(x + 1, None);
            }

            idx[x] = Some((row, col));

            col += 1;
            if col >= BOARD_SIZE {
                col = 0;
                row += 1;
            }
            if row >= BOARD_SIZE {
                row = 0;

                boards.push(b.clone());
                rev_indexes.push(std::mem::take(&mut idx));
            }
        }
    }

    (boards, rev_indexes)
}

fn is_board_complete(board: &Board) -> bool {
    let any_row_complete = board
        .iter()
        .map(|row| row.iter().all(|(_, b)| *b))
        .any(|b| b);
    let any_col_complete = (0..BOARD_SIZE)
        .map(|col| (0..BOARD_SIZE).all(|row| board[row][col].1))
        .any(|b| b);

    any_row_complete || any_col_complete
}

fn score_board(board: &Board) -> usize {
    board
        .iter()
        .flat_map(|row| {
            row.iter()
                .filter_map(|(value, marked)| if *marked { None } else { Some(value) })
        })
        .sum()
}

fn main() {
    let nums = io::stdin().lines().next().unwrap().unwrap();
    let nums: Vec<usize> = nums.split(",").map(|s| s.parse().unwrap()).collect();

    let (mut boards, rev_indexes) = parse_boards();

    let mut completed: HashSet<usize> = HashSet::new();

    // Part two, stop on first only in the first part
    let stop_on_first = env::args().nth(1).unwrap() == "1";

    let mut last_num = 0usize;
    let mut last_board = None;

    'outer: for n in nums {
        last_num = n;

        for (board_idx, (board, idx)) in boards.iter_mut().zip(&rev_indexes).enumerate() {
            last_board = Some(board_idx);

            if let Some(Some((row, col))) = idx.get(n) {
                let (old, _) = board[*row][*col];
                board[*row][*col] = (old, true);

                if !completed.contains(&board_idx) && is_board_complete(&board) {
                    completed.insert(board_idx);

                    if stop_on_first || completed.len() == rev_indexes.len() {
                        break 'outer;
                    }
                }
            }
        }
    }

    if let Some(board_idx) = last_board {
        let score = score_board(&boards[board_idx]);
        println!("{}", score * &last_num);
    }
}
