module Euler.Problem21

(* Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n). If d(a) = b and d(b) = a, where a ≠ b, then a and b
are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.*)

let properDivisors n =
    [1..int(sqrt(float(n)))]
    |> List.filter (fun i -> n % i = 0)
    |> List.collect (fun i -> [i; n/i])
    |> List.filter ((<>) n)
    |> List.distinct

let sumOfProperDivisors = properDivisors >> List.sum

let euler21 n =
    let divisorMap = Map.ofList [for i in 1..n -> (i,sumOfProperDivisors i)]
    let areAmicable a b =
        if a = b then false
        else divisorMap.Item a = b && divisorMap.Item b = a
    let findAmicablePairs a =
        let l = 
            [a+1..n]
            |> List.filter (areAmicable a)
        match l with
        | [] -> []
        | l -> a::l
    [220..n-1]
    |> List.map (findAmicablePairs)
    |> List.filter (List.isEmpty >> not) 
    |> List.collect id
    |> List.sum

let solution = euler21 10000
