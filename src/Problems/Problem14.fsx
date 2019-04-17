module Euler.Problem14

(* The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even) n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1 It can be seen that this sequence
(starting at 13 and finishing at 1) contains 10 terms. Although it has not been
proved yet (Collatz Problem), it is thought that all starting numbers finish at
1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.*)

open System

let collatzLength (n:int64) =
    (Seq.unfold (fun (i:int64) ->
        if i <= 0L then 
            failwith "The input must be at least 1" 
        if i = 1L
            then None 
        else if i % 2L = 0L
            then Some(i,i / 2L) 
        else 
            Some(i,3L * i + 1L)) n
    |> Seq.length) + 1

let euler14 n =
    [1L..n-1L]
    |> Seq.map (fun (i:int64) -> i,(collatzLength i))
    |> Seq.maxBy snd
    |> fst

let solution = euler14 1000000L
