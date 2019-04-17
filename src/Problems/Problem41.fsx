module Euler.Problem41

(* We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
also prime.

What is the largest n-digit pandigital prime that exists?*)

#load "../Helpers.fsx"
open System
open Helpers

let isPandigital num =
    let s = string num
    let n = String.length s
    set s = set [ '1' .. char (string n) ]

let solution =
    Seq.initInfinite (fun i ->
        Permutations.ofList [ 1 .. 9 - i ]
        |> Seq.map (List.map string >> List.reduce (+) >> Int32.Parse)
        |> Seq.filter (int64 >> Prime.isPrime)
        |> Seq.sortDescending
    ) 
    |> Seq.collect id
    |> Seq.find isPandigital
