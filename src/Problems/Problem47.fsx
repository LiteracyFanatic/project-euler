module Euler.Problem47

(*
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors 
each. What is the first of these numbers?*)

#load "../Helpers.fsx"
open Helpers

let isFactor n x =
    n % x = 0

let maxn = 150000

let primes = 
    Prime.primesUpTo maxn 
    |> List.toArray

let primeFactors n =
    let mutable i = 0
    let mutable result = []
    while  primes.[i] < n / 2 do
        if isFactor n primes.[i] then 
            result <- primes.[i] :: result
        i <- i + 1
    result

let solution =
    [|1 .. 150000|]
    |> Array.windowed 4
    |> Array.find (Array.forall (fun x -> List.length (primeFactors x) = 4))
    |> Array.head
