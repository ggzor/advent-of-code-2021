use std::env;
use std::io;

fn main() {
    let desc = io::stdin().lines().next().unwrap().unwrap();
    let parts: Vec<i32> = desc
        .split(|c| matches!(c, '=' | '.' | ',' | ' '))
        .filter_map(|s| s.parse().ok())
        .collect();

    let guess = 300;

    let mut total_max_y = 0;
    let mut hit_count = 0;

    if let [xmin, xmax, ymin, ymax] = parts[..] {
        for xv in 0..=xmax + 1 {
            for yv in ymin - 1..=guess {
                let mut xv = xv;
                let mut yv = yv;
                let mut x = 0;
                let mut y = 0;

                let mut max_y = 0;
                let mut hit = false;

                loop {
                    x += xv;
                    y += yv;

                    xv -= xv.signum();
                    yv -= 1;

                    max_y = max_y.max(y);

                    if xmin <= x && x <= xmax && ymin <= y && y <= ymax {
                        hit = true;
                    }

                    if y < ymin || x > xmax || (xv == 0 && x < xmin) {
                        break;
                    }
                }

                if hit {
                    total_max_y = total_max_y.max(max_y);
                    hit_count += 1;
                }
            }
        }
    }

    if env::args().nth(1).unwrap() == "1" {
        println!("{}", total_max_y);
    } else {
        println!("{}", hit_count);
    }
}
