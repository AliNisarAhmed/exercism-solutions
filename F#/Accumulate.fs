module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list) : 'b list =
    let cons x xs = x :: xs

    let rec accumulate2 f acc =
        function
        | [] -> acc []
        | x :: xs -> accumulate2 f (acc << (cons (f x))) xs


    accumulate2 func id input


// using "difference list" technique: https://blog.ploeh.dk/2015/12/22/tail-recurse/