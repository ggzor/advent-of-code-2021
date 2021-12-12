#![feature(stdin_forwarders)]
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::io;

type Node = str;
type Graph = HashSet<(String, String)>;

fn dfs_one<'a>(cur: &'a Node, g: &'a Graph, seen: &mut HashSet<&'a str>) -> u32 {
    if seen.contains(cur) {
        0
    } else if cur == "end" {
        1
    } else {
        if cur.chars().all(char::is_lowercase) {
            seen.insert(cur);
        }

        let mut result = 0;
        for n in g.iter().filter(|(a, _)| a == cur).map(|(_, b)| b) {
            result += dfs_one(n, g, seen);
        }

        if cur.chars().all(char::is_lowercase) {
            seen.remove(cur);
        }

        result
    }
}

fn dfs_two<'a>(
    cur: &'a Node,
    g: &'a Graph,
    start_seen: bool,
    counts: &mut HashMap<&'a str, u32>,
) -> u32 {
    if (cur == "start" && start_seen)
        || counts.values().filter(|v| **v == 2).count() > 1
        || counts.get(cur).unwrap_or(&0) == &2
    {
        0
    } else if cur == "end" {
        1
    } else {
        if cur.chars().all(char::is_lowercase) {
            *counts.entry(cur).or_default() += 1;
        }

        let mut result = 0;
        for n in g.iter().filter(|(a, _)| a == cur).map(|(_, b)| b) {
            result += dfs_two(n, g, true, counts);
        }

        if cur.chars().all(char::is_lowercase) {
            *counts.entry(cur).or_insert(1) -= 1;
        }

        result
    }
}

fn main() {
    let g: Graph = io::stdin()
        .lines()
        .map(Result::unwrap)
        .flat_map(|s| {
            let (a, b) = s.split_once('-').unwrap();
            let a = a.to_owned();
            let b = b.to_owned();

            [(b.clone(), a.clone()), (a, b)]
        })
        .collect();

    if env::args().nth(1).unwrap() == "1" {
        println!("{}", dfs_one("start", &g, &mut HashSet::new()))
    } else {
        println!("{}", dfs_two("start", &g, false, &mut HashMap::new()))
    }
}
