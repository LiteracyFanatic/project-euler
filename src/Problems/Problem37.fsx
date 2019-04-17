module Euler.Problem37

(* The number 3797 has an interesting property. Being prime itself, it is
possible to continuously remove digits from left to right, and remain prime at
each stage: 3797, 797, 97, and 7. Similarly we can work from right to left:
3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.*)

#load "../Helpers.fsx"
open Helpers

let truncateRight x = 
    x / 10

let truncateLeft x =
    let rec loop a b =
        if a > 9 then
            loop (truncateRight a) (b * 10)
        else 
            x - a * b        
    loop x 1

let getTruncations x = [ 
    let rec loop f x = [
        if x > 9 then
            let x' = f x
            yield x'
            yield! loop f x'
        else 
            () 
    ]
    yield! loop truncateRight x        
    yield! loop truncateLeft x 
]

let isPrimeTruncatable x =
    x |> int64 |> Prime.isPrime 
    && x |> getTruncations |> List.forall (int64 >> Prime.isPrime)

let solution =
    Seq.initInfinite (fun  i -> i + 11)
    |> Seq.filter isPrimeTruncatable
    |> Seq.take 11
    |> Seq.sum
