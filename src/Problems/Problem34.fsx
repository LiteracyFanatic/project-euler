module Euler.Problem34

(* 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their
digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.*)

let determineDigitCount x =
    if x < 10 then 1
    else if x < 100 then 2
    else if x < 1000 then 3
    else if x < 10000 then 4
    else if x < 100000 then 5
    else if x < 1000000 then 6
    else if x < 10000000 then 7
    else if x < 100000000 then 8
    else if x < 1000000000 then 9
    else 10

let convertToArrayOfDigits x =
    let size = determineDigitCount x
    let mutable digits = Array.zeroCreate size
    let mutable value = x
    for index in size - 1 .. -1 .. 0 do
        digits.[index] <- value % 10
        value <- value / 10
    digits

let factorial = function
    | 0 -> 1
    | n -> List.reduce (*) [1 .. n]

let sumOfFactorialOfDigits =
    convertToArrayOfDigits >> Seq.sumBy factorial

let solution =
    [|3 .. 9999999|]
    |> Array.Parallel.choose (fun x -> 
        if x = sumOfFactorialOfDigits x then Some x else None) 
    |> Array.sum
