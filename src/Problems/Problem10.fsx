module Euler.Problem10

(*The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.*)

#load "../Helpers.fsx"
open Helpers

let euler10 n =
    [1L..n] 
    |> List.sumBy (fun i -> if Prime.isPrime i then i else 0L)

let solution = euler10 2000000L
