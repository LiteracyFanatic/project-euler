module Euler.Problem42
open FSharpx.IO

(* The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its
alphabetical position and adding these values we form a word value. For example,
the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a
triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
containing nearly two-thousand common English words, how many are triangle
words?*)

#load "../../.paket/load/net45/main.group.fsx"
open FSharpx
open System
let wordValue =
    String.toUpper
    >> Seq.sumBy (fun c -> int c - 64)

let triangleNumbers =
    Seq.initInfinite ((+) 1)
    |> Seq.map (fun n -> n * (n + 1) / 2)

let isTriangleWord s =
    let wv = wordValue s
    triangleNumbers
    |> Seq.takeWhile (fun n -> n <= wv)
    |> Seq.contains wv

let words =
    IO.File.ReadAllText "src/Problems/Euler42.txt"
    |> String.splitCharWithOptions 
        [|','; '"'|] 
        StringSplitOptions.RemoveEmptyEntries

let solution =
    words
    |> Array.filter isTriangleWord
    |> Array.length
