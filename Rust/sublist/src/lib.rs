use std::cmp::min;

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

// best solution: not mine:

// pub fn sublist<T: PartialEq>(a: &[T], b: &[T]) -> Comparison {
//     match (a.len(), b.len()) {
//         (0, 0) => Comparison::Equal,
//         (0, _) => Comparison::Sublist,
//         (_, 0) => Comparison::Superlist,
//         (m, n) if m > n => {
//             if a.windows(n).any(|w| w == b) {
//                 Comparison::Superlist
//             } else {
//                 Comparison::Unequal
//             }
//         }
//         (m, n) if m < n => {
//             if b.windows(m).any(|w| w == a) {
//                 Comparison::Sublist
//             } else {
//                 Comparison::Unequal
//             }
//         }
//         (_, _) => {
//             if a == b {
//                 Comparison::Equal
//             } else {
//                 Comparison::Unequal
//             }
//         }
//     }
// }

pub fn sublist<T: PartialEq>(_first_list: &[T], _second_list: &[T]) -> Comparison {
    let is_first_sublist = is_sublist(_first_list, _second_list);
    let is_first_superlist = is_sublist(_second_list, _first_list);

    match (is_first_sublist, is_first_superlist) {
        (true, true) => Comparison::Equal,
        (true, false) => Comparison::Sublist,
        (false, true) => Comparison::Superlist,
        _ => Comparison::Unequal,
    }
}

pub fn is_sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> bool {
    let length_first = first_list.len();
    let length_second = second_list.len();

    if length_first == 0 {
        return true;
    }

    if length_second == 0 || length_second < length_first {
        return false;
    }

    let head = &first_list[0];

    for (index, item) in second_list.iter().enumerate() {
        // if there is no space left for the first list
        if length_second - index < length_first {
            return false;
        }

        if item == head {
            let end_index = min(length_second, index + length_first);
            let rest = &second_list[index..end_index];
            if rest == first_list {
                return true;
            }
        }
    }

    false
}
