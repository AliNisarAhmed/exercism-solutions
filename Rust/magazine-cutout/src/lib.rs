use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let note_freq = count_words(note);
    let magazine_freq = count_words(magazine);

    for (k, count) in note_freq.iter() {
        if !magazine_freq.contains_key(k) || magazine_freq.get(k).unwrap() < count {
            return false;
        }
    }

    true
}

fn count_words<'a>(words: &[&'a str]) -> HashMap<&'a str, usize> {
    words.iter().fold(HashMap::new(), |mut acc, x| {
        *acc.entry(*x).or_insert(1) += 1;
        acc
    })
}
