module Euler.Problem12

(* The sequence of triangle numbers is generated by adding the natural numbers.
So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first
ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1 3: 1,3 6: 1,2,3,6 10: 1,2,5,10 15: 1,3,5,15 21: 1,3,7,21 28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred
divisors?*)

let triangleNumbers =
    Seq.initInfinite (fun n -> n * (n + 1) / 2)
    |> Seq.skip 1

let numberOfFactors n =
    [1L..int64(ceil(sqrt(float n) + 1.0))]
    |> List.sumBy (fun i -> 
        if i * i = n then 1
        else if n % i = 0L then 2 
        else 0)

let euler12 n =        
    triangleNumbers 
    |> Seq.find (fun i -> (numberOfFactors (int64 i)) > n)

let solution = euler12 500 
