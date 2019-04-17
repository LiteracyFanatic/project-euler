module Euler.Problem1

// If we list all the natural numbers below 10 that are multiples of 3 or 5, we
// get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the
// multiples of 3 or 5 below 1000.

let sumOfMultiplesOf3Or5 =
    List.sumBy (fun i -> if i % 3 = 0 || i % 5 = 0 then i else 0)

let solution = [0..999] |> sumOfMultiplesOf3Or5
