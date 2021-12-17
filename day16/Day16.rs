#![feature(stdin_forwarders, array_chunks)]
use std::env;
use std::io;

#[derive(Debug)]
enum Payload {
    Literal(i64),
    Operator(Vec<Packet>),
}

#[derive(Debug)]
struct Packet(u64, u64, Payload);

// Just a bit of overhead
type Input<'a> = &'a mut dyn Iterator<Item = char>;

fn parse_bin(it: Input) -> u64 {
    u64::from_str_radix(&it.collect::<String>(), 2).unwrap()
}

fn parse(it: Input) -> Packet {
    let version: u64 = parse_bin(&mut it.take(3));
    let ty: u64 = parse_bin(&mut it.take(3));

    if ty == 4 {
        let mut parts: String = String::new();
        loop {
            let should_break = it.next().unwrap() == '0';

            parts.extend(it.take(4));

            if should_break {
                break;
            }
        }
        Packet(
            version,
            ty,
            Payload::Literal(parse_bin(&mut parts.chars()) as i64),
        )
    } else {
        match it.next().unwrap() {
            '0' => {
                let subpack_len = parse_bin(&mut it.take(15)) as usize;
                let mut subiter = it.take(subpack_len).peekable();

                let mut subpacks: Vec<Packet> = Vec::new();
                while let Some(_) = subiter.peek() {
                    subpacks.push(parse(&mut subiter));
                }

                Packet(version, ty, Payload::Operator(subpacks))
            }
            _ => {
                let subpack_parts = parse_bin(&mut it.take(11)) as usize;
                let mut subpacks: Vec<Packet> = Vec::new();

                for _ in 0..subpack_parts {
                    subpacks.push(parse(it));
                }

                Packet(version, ty, Payload::Operator(subpacks))
            }
        }
    }
}

fn sum_versions(p: &Packet) -> u64 {
    match p {
        Packet(v, _, Payload::Literal(_)) => *v,
        Packet(v, _, Payload::Operator(subpacks)) => {
            *v + subpacks.iter().map(sum_versions).sum::<u64>()
        }
    }
}

fn eval(p: &Packet) -> i64 {
    match p {
        Packet(_, _, Payload::Literal(x)) => *x,
        Packet(_, ty, Payload::Operator(subpacks)) => {
            let mut subvalues = subpacks.iter().map(eval);

            fn with_next_two(op: fn(i64, i64) -> bool, it: &mut dyn Iterator<Item = i64>) -> i64 {
                op(it.next().unwrap(), it.next().unwrap()) as i64
            }

            let f: fn(&mut dyn Iterator<Item = i64>) -> i64 = match ty {
                0 => |it| it.sum(),
                1 => |it| it.product(),
                2 => |it| it.min().unwrap(),
                3 => |it| it.max().unwrap(),
                5 => |it| with_next_two(|x, y| x > y, it),
                6 => |it| with_next_two(|x, y| x < y, it),
                7 => |it| with_next_two(|x, y| x == y, it),
                _ => unreachable!(),
            };

            f(&mut subvalues)
        }
    }
}

fn main() {
    let input = io::stdin().lines().next().unwrap().unwrap();
    let mut input = input.chars().flat_map(|c| {
        format!("{:04b}", c.to_digit(16).unwrap())
            .chars()
            .collect::<Vec<_>>()
    });

    let result = parse(&mut input);
    // println!("{:?}", result);

    if env::args().nth(1).unwrap() == "1" {
        println!("{}", sum_versions(&result));
    } else {
        println!("{}", eval(&result));
    }
}
