module Euler.Problem46

(* It was proposed by Christian Goldbach that every odd composite number can be
written as the sum of a prime and twice a square.

9 = 7 + 2×1^2 
15 = 7 + 2×2^2 
21 = 3 + 2×3^2 
25 = 7 + 2×3^2 
27 = 19 + 2×2^2 
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime
and twice a square?*)

#load "../Helpers.fsx"
open Helpers

let isComposite =
    not << Prime.isPrime

let isOdd n =
    n % 2 <> 0

let goldbach n =
    let squares = 
        [1 .. int (sqrt (float n / 2.))]
        |> List.map (fun x -> x * x)
    
    List.exists (fun s -> Prime.isPrime (int64 (n - 2 * s))) squares

let solution =
    Seq.initInfinite ((+) 3)
    |> Seq.filter (fun n -> isOdd n && isComposite (int64 n))
    |> Seq.find (not << goldbach)
