use std::env;
use std::io;

const STARTING: &str = "([{<";
const ENDING: &str = ")]}>";
fn main() {
    let matching =
        |src: &str, c: char, target: &str| src.find(c).and_then(|i| target.chars().nth(i));

    type Partial = Vec<char>;
    type Wrong = char;

    let mut parse_results: Vec<Result<Partial, Wrong>> = Vec::new();

    for l in io::stdin().lines().map(Result::unwrap) {
        let mut wrong = false;
        let mut stack: Partial = Vec::with_capacity(l.len());

        for c in l.chars() {
            if STARTING.contains(c) || stack.is_empty() {
                stack.push(c)
            } else if let Some(prev) = stack.pop() {
                if prev != matching(ENDING, c, STARTING).unwrap() {
                    parse_results.push(Err(c));
                    wrong = true;
                    break;
                }
            }
        }

        if !wrong {
            parse_results.push(Ok(stack));
        }
    }

    if env::args().nth(1).unwrap() == "1" {
        let result: u32 = parse_results
            .iter()
            .filter_map(|r| r.as_ref().err())
            .map(|c| match c {
                ')' => 3,
                ']' => 57,
                '}' => 1197,
                '>' => 25137,
                _ => 0,
            })
            .sum();

        println!("{}", result);
    } else {
        let mut scores: Vec<usize> = parse_results
            .iter()
            .filter_map(|v| v.as_ref().ok())
            .map(|v| {
                v.iter()
                    .rev()
                    .map(|c| matching(STARTING, *c, ENDING).unwrap())
                    .map(|c| ENDING.find(c).unwrap() + 1)
                    .fold(0, |a, b| a * 5 + b)
            })
            .collect();

        scores.sort_unstable();

        println!("{}", scores[scores.len() / 2]);
    }
}
