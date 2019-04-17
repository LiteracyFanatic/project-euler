module Euler.Problem5

(* 2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?*)

let isEvenlyDivisibleByRange l n =
    l |> List.forall (fun i -> n % i = 0)

let euler5 n =
    Seq.initInfinite (fun i -> (i + 1) * n)
    |> Seq.find (isEvenlyDivisibleByRange [2..n])

let solution = euler5 20
