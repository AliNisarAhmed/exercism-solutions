// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(PartialEq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn turn_right(self) -> Self {
        match self {
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
            Direction::North => Direction::East,
        }
    }

    fn turn_left(self) -> Self {
        match self {
            Direction::East => Direction::North,
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
        }
    }
}

pub struct Robot {
    d: Direction,
    coords: (i32, i32),
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Robot { d, coords: (x, y) }
    }

    #[must_use]
    pub fn turn_right(self) -> Self {
        Robot {
            d: self.d.turn_right(),
            ..self
        }
    }

    #[must_use]
    pub fn turn_left(self) -> Self {
        Robot {
            d: self.d.turn_left(),
            ..self
        }
    }

    #[must_use]
    pub fn advance(self) -> Self {
        let mut y_offset = 0;
        let mut x_offset = 0;

        match self.d {
            Direction::East => x_offset += 1,
            Direction::West => x_offset -= 1,
            Direction::North => y_offset += 1,
            Direction::South => y_offset -= 1,
        }

        Robot {
            coords: (self.coords.0 + x_offset, self.coords.1 + y_offset),
            ..self
        }
    }

    #[must_use]
    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |result, c| match c {
            'A' => result.advance(),
            'L' => result.turn_left(),
            'R' => result.turn_right(),
            _ => panic!("unknown instruction"),
        })
    }

    pub fn position(&self) -> (i32, i32) {
        self.coords
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
