#![feature(stdin_forwarders)]
use std::collections::HashMap;
use std::env;
use std::io;

type Segment = u32;

fn encode_str(s: &str) -> Segment {
    s.chars()
        .filter(|c| ('a'..='g').contains(c))
        .map(|c| c as u8 - 'a' as u8)
        .fold(0, |acc, n| acc | (1 << n))
}

#[allow(dead_code)]
fn decode_segment(s: Segment) -> String {
    ('a'..='g').fold(String::new(), |mut result, c| {
        if s & (1 << c as Segment - 'a' as Segment) != 0 {
            result.push(c);
        }
        result
    })
}

fn main() {
    let nums: Vec<(Vec<Segment>, Vec<Segment>)> = io::stdin()
        .lines()
        .map(|s| {
            let mut r: Vec<_> = s
                .unwrap()
                .split('|')
                .map(|part| part.split_whitespace().map(encode_str).collect())
                .collect();

            let target = r.pop().unwrap();
            let digits = r.pop().unwrap();

            (digits, target)
        })
        .collect();

    let known_lens = HashMap::from([(2, '1'), (3, '7'), (4, '4'), (7, '8')]);

    if env::args().nth(1).unwrap() == "1" {
        let count = nums
            .iter()
            .flat_map(|(_, target)| {
                target
                    .iter() //
                    .filter(|s| known_lens.contains_key(&s.count_ones()))
            })
            .count();
        println!("{}", count);
    } else {
        let mut total = 0;
        for (digits, target) in nums {
            let mut known_digits = HashMap::new();

            for d in &digits {
                if let Some(digit) = known_lens.get(&d.count_ones()) {
                    known_digits.insert(digit, d);
                }
            }

            let intersect_digits_of_len = |len: u32| {
                digits
                    .iter()
                    .filter(|d| d.count_ones() == len)
                    .cloned()
                    .reduce(|a, b| a & b)
                    .unwrap_or(0)
            };

            let segs_adg = intersect_digits_of_len(5);
            let segs_abfg = intersect_digits_of_len(6);

            let seg_d = segs_adg & !segs_abfg;

            let dig_0 = known_digits[&'8'] & !seg_d;
            let dig_3 = segs_adg | known_digits[&'1'];
            let dig_5 = segs_abfg | seg_d;

            let dig_2 = segs_adg | (known_digits[&'8'] & !dig_5);
            let dig_9 = dig_5 | known_digits[&'1'];
            let dig_6 = dig_5 | (known_digits[&'8'] & !dig_9);

            let mut mapping = HashMap::new();

            for (k, v) in known_digits {
                mapping.insert(v, k);
            }
            mapping.insert(&dig_0, &'0');
            mapping.insert(&dig_3, &'3');
            mapping.insert(&dig_5, &'5');

            mapping.insert(&dig_2, &'2');
            mapping.insert(&dig_9, &'9');
            mapping.insert(&dig_6, &'6');

            let number: u32 = target
                .iter()
                .map(|s| mapping[s])
                .fold(String::new(), |mut s, c| {
                    s.push(*c);
                    s
                })
                .parse()
                .unwrap_or(0);

            total += number;
        }
        println!("{}", total);
    }
}
