use std::collections::HashMap;

fn main() {
    let mut left = vec![];
    let mut right = vec![];
    let mut counts = HashMap::new();
    for line in std::fs::read_to_string("./input/day1.txt")
        .unwrap()
        .lines()
    {
        let [x, y] = line
            .split(' ')
            .filter(|s| !s.is_empty())
            .map(|s| s.parse::<u32>())
            .collect::<Result<Vec<_>, _>>()
            .unwrap()[..]
        else {
            continue;
        };
        left.push(x);
        right.push(y);
        if let Some(c) = counts.get_mut(&y) {
            *c += 1;
        } else {
            counts.insert(y, 1);
        }
    }
    left.sort();
    right.sort();

    println!(
        "{}",
        left.iter()
            .zip(right.iter())
            .map(|(l, r)| (*l as i32 - *r as i32).abs())
            .sum::<i32>()
    );
    println!(
        "{}",
        left.iter()
            .map(|l| counts.get(l).copied().unwrap_or_default() * l)
            .sum::<u32>()
    );
}
