#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack: Vec<i32> = vec![];

    for item in inputs {
        match item {
            CalculatorInput::Value(v) => stack.push(*v),
            op => {
                if stack.len() < 2 {
                    return None;
                }

                let v1 = stack.pop().unwrap();
                let v2 = stack.pop().unwrap();

                let result = match op {
                    CalculatorInput::Add => v2 + v1,
                    CalculatorInput::Subtract => v2 - v1,
                    CalculatorInput::Multiply => v2 * v1,
                    CalculatorInput::Divide => v2 / v1,
                    _ => 0,
                };

                stack.push(result);
            }
        }
    }

    if stack.len() != 1 {
        return None;
    }

    stack.pop()
}
