module Euler.Problem50

(*
The prime 41, can be written as the sum of six consecutive primes: 41 = 2 + 3 +
5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below
one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime,
contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most
consecutive primes?*)

#load "../Helpers.fsx"
open Helpers

let solution =
    let primesBelowOneMillion =
        Prime.getPrimes 1000000
        |> Seq.toArray

    let mutable maxCount = 0
    let mutable sol = 0

    for i in 0 .. primesBelowOneMillion.Length - 1 do
        let mutable sum = 0
        let mutable primeCount = 0
        
        let rec loop j =
            if sum < 1000000 && j < primesBelowOneMillion.Length then
                sum <- sum + primesBelowOneMillion.[j]

                primeCount <- primeCount + 1

                if primeCount > maxCount && Prime.isPrime sum then 
                    maxCount <- primeCount
                    sol <- sum
                loop (j + 1)
        loop i

    sol
    