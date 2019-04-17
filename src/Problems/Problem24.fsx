module Euler.Problem24

(* A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are
listed numerically or alphabetically, we call it lexicographic order. The
lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,
6, 7, 8 and 9?*)

open System

let factorial n = List.reduce (*) [1..n]

let findLeadingElement ((l: 'a list), permNumber) =
    let permsPerItem = factorial (l.Length - 1)
    let (n, r) = Math.DivRem(permNumber,permsPerItem)
    let n' = if r = 0 then n - 1 else n 
    l.[n'], (List.filter ((<>) l.[n']) l, permNumber - (n') * permsPerItem)

let findPerm<'T when 'T : equality> =
    List.unfold (function
        | [], permNumber -> None
        | (l:'T list), permNumber ->
            Some(findLeadingElement (l,permNumber)))
    >> List.map (fun i -> i.ToString()) 
    >> List.reduce (+)

let euler24 (l, n:int) = findPerm ((List.sort l), n)

let solution = euler24 ([0..9],1000000)
