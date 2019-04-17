module Euler.Problem25

(* The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1. Hence the first 12 terms will be:

F1 = 1 F2 = 1 F3 = 2 F4 = 3 F5 = 5 F6 = 8 F7 = 13 F8 = 21 F9 = 34 F10 = 55 F11 =
89 F12 = 144 The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000
digits?*)

let fib =
    Seq.unfold (fun (a, b) -> Some(a + b, (b, a + b))) (0I, 1I)
    |> Seq.append (seq { yield 1I })

let numberOfDigits n = n.ToString().ToCharArray().Length

let solution =
    1 + Seq.findIndex (fun i -> (numberOfDigits i) = 1000) fib
