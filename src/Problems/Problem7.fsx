module Euler.Problem7
(*By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
that the 6th prime is 13.

What is the 10 001st prime number?*)

#load "../Helpers.fsx"
open System
open Helpers

let euler7 n =
    Seq.initInfinite int64
    |> Seq.filter Prime.isPrime
    |> Seq.item (n - 1)

let solution = euler7 10001
