module Euler.Problem4

(* A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.*)

let isPalindrome n = 
    let original = [for c in n.ToString() -> c]
    original = List.rev original

let solution =
    [
        for x in [999..-1..100] do
            yield seq {
                for y in [999..-1..100] do
                yield x * y
            }
    ]
    |> List.map (Seq.tryFind isPalindrome)
    |> List.choose id
    |> List.max
