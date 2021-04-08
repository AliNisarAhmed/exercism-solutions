module Bob

open System

let private everypred preds = function
    | xs -> Seq.fold (fun r p -> r && p xs) true preds

let silence = Seq.isEmpty

let shouting s =
    match Seq.filter Char.IsLetter s with
    | s when silence s -> false
    | s -> Seq.forall Char.IsUpper s

let question s =
    match Seq.tryLast s with
    | Some c -> c = '?'
    | _ -> false

let forcefulQuestion = everypred [ question; shouting ]

let response (input: string) =
    match input.Trim() with
    | s when forcefulQuestion s -> "Calm down, I know what I'm doing!"
    | s when shouting s -> "Whoa, chill out!"
    | s when question s -> "Sure."
    | s when silence s -> "Fine. Be that way!"
    | _ -> "Whatever."