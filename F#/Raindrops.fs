module Raindrops

open System

let convert (number: int) : string =
    let numbers =
        [ (3, "Pling")
          (5, "Plang")
          (7, "Plong") ]

    let result =
        List.fold (fun acc (n, s) -> if number % n = 0 then acc + s else acc) "" numbers

    if String.length (result) = 0 then
        number.ToString()
    else
        result
