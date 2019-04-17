module Euler.Problem23

(* A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.*)

#load "Problem21.fsx"
open Euler

let solution =
    let divisorMap = Map.ofList [for i in 1..28123 -> (i, Problem21.sumOfProperDivisors i)]
    let abundantNumbers = 
        divisorMap 
        |> Map.filter (fun k v -> v > k)
        |> Map.toList
        |> List.map fst
    let abundantSums =
        abundantNumbers
        |> List.collect (fun i -> 
            (abundantNumbers |> List.skipWhile ((<>) i)) 
            |> List.map ((+) i))
        |> List.filter ((>=) 28123)
        |> List.distinct
    [1..28123]
    |> List.filter (fun i -> not (List.contains i abundantSums)) 
    |> List.sum
