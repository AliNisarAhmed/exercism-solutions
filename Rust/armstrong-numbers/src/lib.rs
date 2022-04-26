pub fn is_armstrong_number(num: u32) -> bool {
    let digs = digits(num);
    let pow = digs.len() as u32;

    digs.iter().map(|n| n.pow(pow)).sum::<u32>() == num
}

fn digits(num: u32) -> Vec<u32> {
    let mut x = num;
    std::iter::from_fn(move || {
        if x == 0 {
            None
        } else {
            let current = x % 10;
            x /= 10;
            Some(current)
        }
    })
    .collect()
}
