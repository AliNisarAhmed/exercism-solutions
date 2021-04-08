module BeerSong

open System
open FSharp.Collections

let writeFirstClause =
    function
    | bottles when bottles > 1 -> $"{bottles} bottles of beer on the wall, {bottles} bottles of beer."
    | bottles when bottles = 1 -> $"{bottles} bottle of beer on the wall, {bottles} bottle of beer."
    | _ -> $"No more bottles of beer on the wall, no more bottles of beer."

let writeSecondClause =
    function
    | bottles when bottles > 2 -> $"Take one down and pass it around, {bottles - 1} bottles of beer on the wall."
    | bottles when bottles = 2 -> $"Take one down and pass it around, {bottles - 1} bottle of beer on the wall."
    | bottles when bottles = 1 -> "Take it down and pass it around, no more bottles of beer on the wall."
    | _ -> "Go to the store and buy some more, 99 bottles of beer on the wall."



let recite (startBottles: int) (takeDown: int) =
    let seq = seq { 1 .. takeDown }

    let folder acc x =
        let firstClause = writeFirstClause (startBottles + 1 - x)
        let secondClause = writeSecondClause (startBottles + 1 - x)
        Seq.append acc [ firstClause; secondClause; ""; ]

    Seq.fold folder Seq.empty seq
      |> Seq.truncate (3 * takeDown - 1)
      |> Seq.toList
