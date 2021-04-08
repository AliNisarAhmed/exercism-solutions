module RobotSimulator

open FSharp.Collections

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

let create direction position =
    { direction = direction
      position = position }


let turnRight =
    function
    | North -> East
    | East -> South
    | South -> West
    | _ -> North

let turnLeft =
    function
    | North -> West
    | West -> South
    | South -> East
    | _ -> North

let advance currentDir (e, n) =
    match currentDir with
    | North -> (e, n + 1)
    | South -> (e, n - 1)
    | East -> (e + 1, n)
    | _ -> (e - 1, n)


let changeDirection ins dir =
    match ins with
    | 'R' -> turnRight dir
    | 'L' -> turnLeft dir
    | _ -> dir


let move instructions robot =
    let folder r ins =
        match ins with
        | 'A' ->
            let np = advance r.direction r.position
            create r.direction np
        | _ ->
            let nd = changeDirection ins r.direction
            create nd r.position

    Seq.fold folder robot instructions
