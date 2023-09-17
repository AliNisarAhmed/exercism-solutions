pub fn reply(message: &str) -> &str {
    let message = message.trim();
    let question = is_question(message);
    let yelling = is_yelling(message);

    if message.is_empty() {
        return "Fine. Be that way!";
    }

    if question && yelling {
        return "Calm down, I know what I'm doing!";
    }

    if question {
        return "Sure.";
    }

    if yelling {
        return "Whoa, chill out!";
    }

    "Whatever."
}

fn is_question(msg: &str) -> bool {
    msg.ends_with('?')
}

fn has_alphabet(msg: &str) -> bool {
    msg.chars().any(|x| x.is_alphabetic())
}

fn is_yelling(msg: &str) -> bool {
    has_alphabet(msg) && msg.to_ascii_uppercase() == msg
}
