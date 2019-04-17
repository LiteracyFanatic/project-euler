module Euler.Problem32

(* We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through
5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can
be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only
include it once in your sum.*)

#load "../Helpers.fsx"
open Helpers

let chooseValidProducts (l: int list) =
    let toInt = List.map string >> List.reduce (+) >> int

    let a = l.[..1] |> toInt
    let b = l.[2..4] |> toInt
    let c = l.[5..] |> toInt
    let d = l.[..0] |> toInt
    let e = l.[1..4] |> toInt
    let f = l.[5..] |> toInt

    if a * b = c then Some c
    else if  d * e = f then Some f
    else None

let solution =
    Permutations.ofList [1..9]
    |> Seq.choose chooseValidProducts
    |> Seq.distinct
    |> Seq.sum
