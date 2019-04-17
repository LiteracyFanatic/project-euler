module Euler.Problem9

(* A Pythagorean triplet is a set of three natural numbers, a < b < c, for
which, a^2 + b^2 = c^2. For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find
the product abc.*)

let square x = x * x

let primitiveTriplets maxSum =
    seq {
        for y in 1..maxSum do
            for x in y+1..maxSum-1 do
                let a = (square x) - (square y)
                let b = 2 * x * y
                let c = (square x) + (square y)
                yield a,b,c
    }
    |> Seq.takeWhile (fun (a,b,c) -> a + b + c <= maxSum )

let multipleTriplets maxSum (a,b,c)  =
    Seq.unfold (fun i -> Some((a * i,b * i,c * i),(i + 1))) 1
    |> Seq.takeWhile (fun (a,b,c) -> a + b + c <= maxSum )

let euler9 sum =
    primitiveTriplets sum
    |> Seq.collect (multipleTriplets sum)
    |> Seq.find (fun (a,b,c) -> a + b + c = sum)
    |>  (fun (a,b,c) -> a * b * c)

let solution = euler9 1000
