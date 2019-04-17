module Euler.Problem20

(* Find the sum of the digits in the number 100!*)

#load "Problem16.fsx"
open Euler

let euler20 n =
    [1..n]
    |> List.map Problem16.numToList
    |> List.reduce Problem16.multLists
    |> List.sum

let solution = euler20 100
