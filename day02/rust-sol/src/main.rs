pub use std::{
    collections::HashSet,
    io::{self, Read},
};

use anyhow::{Context, Result};

fn visit_in_range(lo: i128, mut hi: i128, chunks: u32, callback: &mut impl FnMut(i128)) {
    let lo_digits = lo.ilog10() + 1;
    let hi_digits = hi.ilog10() + 1;

    if hi_digits < chunks {
        return;
    }

    if lo_digits != hi_digits {
        let lo = i128::pow(10, lo_digits);
        visit_in_range(lo, hi, chunks, callback);
        hi = lo - 1;
    }

    let digits = lo_digits.next_multiple_of(chunks) / chunks;

    let start = i128::pow(10, digits - 1);
    let end = i128::pow(10, digits);

    for part in start..end {
        let mut composition = 0;
        for pow in 0..chunks {
            composition += part * i128::pow(10, pow * digits);
        }
        if (lo..=hi).contains(&composition) {
            callback(composition);
        }
    }
}

fn main() -> Result<()> {
    let pairs = get_pairs()?;

    // part 1
    let mut sum = 0;
    for &(lo, hi) in &pairs {
        visit_in_range(lo, hi, 2, &mut |x| {
            sum += x;
        });
    }
    println!("sum = {sum}");

    // part 2
    let mut vals = HashSet::new();
    for &(lo, hi) in &pairs {
        for i in 2..200 {
            visit_in_range(lo, hi, i, &mut |x| {
                vals.insert(x);
            });
        }
    }
    let sum = vals.into_iter().sum::<i128>();
    println!("sum = {sum}");

    Ok(())
}

fn get_pairs() -> Result<Vec<(i128, i128)>> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    buf.trim()
        .split(',')
        .map(|pair| {
            let (a, b) = pair
                .split_once('-')
                .with_context(|| format!("no dash in {pair}"))?;
            let a = a.parse::<i128>()?;
            let b = b.parse::<i128>()?;
            Ok((a, b))
        })
        .collect::<Result<Vec<_>>>()
}
