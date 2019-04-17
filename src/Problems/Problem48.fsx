module Euler.Problem48

(* 
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.*)

open System.Numerics

let solution =
    [1I .. 1000I]
    |> List.sumBy (fun n -> BigInteger.Pow(n, int n))
    |> string
    |> Seq.rev
    |> Seq.take 10
    |> Seq.rev
    |> Seq.map string
    |> Seq.reduce (+)
