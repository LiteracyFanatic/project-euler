module Euler.Problem33

(* The fraction 49/98 is a curious fraction, as an inexperienced mathematician
in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than
one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find
the value of the denominator.*)

let falseSimplify num den =
    let getDigits =
        string
        >> Seq.map (string >> int)
        >> Seq.toList

    let [numLeft; numRight] = getDigits num
    let [denLeft; denRight] = getDigits den

    let validate a b =
        if float a / float b = float num / float den then 
            Some (a, b)
        else 
            None

    if numLeft = denRight then
        validate numRight denLeft
    else if numRight = denLeft then
        validate numLeft denRight
    else 
        None

let greatestCommonFactor a b =
    List.find (fun x -> a % x = 0 && b % x = 0) [min a b .. -1 .. 1]

let simplifyFraction num den =
    let gcf = greatestCommonFactor num den
    num / gcf, den / gcf

let solution =
    [ for num in 10 .. 98 do
        for den in num + 1 .. 99 ->
            falseSimplify num den ]
    |> List.choose id
    |> List.reduce (fun (a, b) (c, d) -> a * c, b * d)
    ||> simplifyFraction
    |> snd
