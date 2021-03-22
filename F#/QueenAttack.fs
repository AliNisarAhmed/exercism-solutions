
module QueenAttack
open System

let create ((row, col): int * int) : bool =
    row >= 0 && row < 8 && col >= 0 && col < 8

let canAttack ((r1, c1): int * int) ((r2, c2): int * int) =
    r1 = r2
    || c1 = c2
    || (Math.Abs (c2 - c1) = Math.Abs (r2 - r1))
