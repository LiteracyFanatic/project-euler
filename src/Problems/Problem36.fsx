module Euler.Problem36

(* The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in
base 10 and base 2.

(Please note that the palindromic number, in either base, may not include
leading zeros.)*)

#load "../../.paket/load/net45/main.group.fsx"
open FSharpx

let isPalindrome x = 
    x = List.rev x

let intToBinary i =
    let rec loop bits i =
        match i with
        | 0 | 1 -> i :: bits
        | _ ->
            let bit = i % 2
            loop (bit :: bits) (i / 2)
    loop [] i        

let solution =
    [1 .. 999999]
    |> List.filter (string >> String.toCharArray >> Array.toList >> isPalindrome)
    |> List.filter (intToBinary >> isPalindrome)
    |> List.sum