use std::io::Read;

use anyhow::Result;

fn main() -> Result<()> {
    let mut input = Vec::new();
    std::io::stdin().read_to_end(&mut input)?;
    let mut input = input
        .split_mut(|&b| b == b'\n')
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>();

    let mut picked_any = true;
    let mut total = 0;
    while picked_any {
        let mut picked = 0;
        for row in 0..input.len() {
            for col in 0..input[0].len() {
                if input[row][col] != b'@' {
                    continue;
                }
                let mut count = 0;
                for dr in -1..=1 {
                    for dc in -1..=1 {
                        if dr == 0 && dc == 0 {
                            continue;
                        }

                        let r = row as isize + dr;
                        let c = col as isize + dc;
                        if r < 0
                            || r >= input.len() as isize
                            || c < 0
                            || c >= input[0].len() as isize
                        {
                            continue;
                        }

                        count += usize::from(matches!(input[r as usize][c as usize], b'@' | b'&'));
                    }
                }
                if count < 4 {
                    picked += 1;
                    input[row][col] = b'&';
                }
            }
        }

        picked_any = picked != 0;
        total += picked;

        for line in &mut input {
            for cell in &mut **line {
                if *cell == b'&' {
                    *cell = b'.';
                }
            }
        }

        println!("picked = {}", picked);
    }

    println!("total = {}", total);

    Ok(())
}
