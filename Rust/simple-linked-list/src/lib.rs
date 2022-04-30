use std::iter::FromIterator;
use std::mem;

pub struct Node<T> {
    data: T,
    next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

pub struct SimpleLinkedList<T> {
    head: Link<T>,
}

impl<T> SimpleLinkedList<T> {
    pub fn new() -> Self {
        SimpleLinkedList { head: None }
    }

    // You may be wondering why it's necessary to have is_empty()
    // when it can easily be determined from len().
    // It's good custom to have both because len() can be expensive for some types,
    // whereas is_empty() is almost always cheap.
    // (Also ask yourself whether len() is expensive for SimpleLinkedList)
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn len(&self) -> usize {
        let mut length = 0;
        let mut current = &self.head;

        while let Some(n) = current {
            length += 1;
            current = &n.next;
        }

        length
    }

    pub fn push(&mut self, _element: T) {
        let new_node = Some(Box::new(Node {
            data: _element,
            next: self.head.take(),
        }));

        self.head = new_node;
    }

    pub fn pop(&mut self) -> Option<T> {
        // match self.head.take() {
        //     None => None,
        //     Some(v) => {
        //         self.head = v.next;
        //         Some(v.data)
        //     }
        // }

        self.head.take().and_then(|v| {
            self.head = v.next;
            Some(v.data)
        })
    }

    pub fn peek(&self) -> Option<&T> {
        match &self.head {
            None => None,
            Some(v) => Some(&v.data),
        }
    }

    #[must_use]
    pub fn rev(self) -> SimpleLinkedList<T> {
        let mut result = SimpleLinkedList::new();

        let mut current = self.head;

        while let Some(n) = current {
            current = n.next;
            result.push(n.data)
        }

        result
    }
}

impl<T> FromIterator<T> for SimpleLinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(_iter: I) -> Self {
        let mut result = SimpleLinkedList::new();

        for i in _iter {
            result.push(i);
        }

        result
    }
}

// In general, it would be preferable to implement IntoIterator for SimpleLinkedList<T>
// instead of implementing an explicit conversion to a vector. This is because, together,
// FromIterator and IntoIterator enable conversion between arbitrary collections.
// Given that implementation, converting to a vector is trivial:
//
// let vec: Vec<_> = simple_linked_list.into_iter().collect();
//
// The reason this exercise's API includes an explicit conversion to Vec<T> instead
// of IntoIterator is that implementing that interface is fairly complicated, and
// demands more of the student than we expect at this point in the track.

impl<T> From<SimpleLinkedList<T>> for Vec<T> {
    fn from(mut _linked_list: SimpleLinkedList<T>) -> Vec<T> {
        let mut res = vec![];

        while let Some(v) = _linked_list.pop() {
            res.push(v);
        }

        res.reverse();
        res
    }
}
