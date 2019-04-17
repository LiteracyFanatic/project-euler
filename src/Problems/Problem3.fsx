module Euler.Problem3

(* The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?*)

#load "../Helpers.fsx"
open Helpers

let solution = Prime.largestPrimeFactor 600851475143L
