module Euler.Problem30

(* Find the sum of all the numbers that can be written as the sum of fifth
powers of their digits.*)

open System

let numToList (s:int) =
    s.ToString().ToCharArray()
    |> Array.toList
    |> List.map (string >> Int32.Parse)

let test num =
    numToList num
    |> List.sumBy (fun d -> d*d*d*d*d)
    |> (=) num

let solution =
    [2..999999]
    |> List.filter test
    |> List.sum
