module Euler.Problem35

(* The number, 197, is called a circular prime because all rotations of the
digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million?*)

#load "../../.paket/load/net45/main.group.fsx"
#load "../Helpers.fsx"
open FSharpx
open Helpers

let toDigits =
    string >> Seq.map (string >> int) >> Seq.toArray

let permutationsOfInt x =
    let digits = toDigits x
    let len = Array.length digits

    [|0 .. len - 1|]
    |> Array.map (fun n ->
        Array.permute (fun i -> (i + n) % len) digits
        |> (Array.map string >> Array.reduce (+) >> int)
    ) 

let isCircularPrime =
    permutationsOfInt >> Array.forall (int64 >> Prime.isPrime)

let solution =
    [|1 .. 999999|]
    |> Array.Parallel.choose (fun x -> if int64 x |> Prime.isPrime then Some x else None)
    |> Array.Parallel.choose (isCircularPrime >> Option.ofBool)
    |> Array.length
