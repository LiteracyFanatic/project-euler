module Euler.Problem17

(* If all the numbers from 1 to 1000 (one thousand) inclusive were written out
in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with British
usage.*)

open System

module Digit =

    type Digit = 
        | Zero 
        | One 
        | Two 
        | Three 
        | Four
        | Five 
        | Six 
        | Seven 
        | Eight 
        | Nine

    let create = function
        | 0 -> Some Zero
        | 1 -> Some One
        | 2 -> Some Two
        | 3 -> Some Three
        | 4 -> Some Four
        | 5 -> Some Five
        | 6 -> Some Six
        | 7 -> Some Seven
        | 8 -> Some Eight
        | 9 -> Some Nine
        | _ -> None
        

open Digit

let numToDigits n =
    [for c in n.ToString() -> c.ToString()]
    |> List.map (Int32.Parse >> Digit.create)
    |> List.map (fun d -> d.Value)

let digitToWord = function
    | Zero -> "Zero"
    | One -> "one"
    | Two -> "two"
    | Three -> "three"
    | Four -> "four"
    | Five -> "five"
    | Six -> "six"
    | Seven -> "seven"
    | Eight -> "eight"
    | Nine -> "nine"

let twoDigitNumberToWord = function
    | Zero,b -> (digitToWord b)
    | One,b ->
        match b with
        | Zero -> "ten"
        | One -> "eleven"
        | Two -> "twelve"
        | Three -> "thirteen"
        | Four -> "fourteen"
        | Five -> "fifteen"
        | Six -> "sixteen"
        | Seven -> "seventeen"
        | Eight -> "eighteen"
        | Nine -> "nineteen"
    | Two,Zero -> "twenty"
    | Two,b -> "twenty-" + (digitToWord b)
    | Three,Zero -> "thirty"
    | Three,b -> "thirty-" + (digitToWord b)
    | Four,Zero -> "forty"
    | Four,b -> "forty-" + (digitToWord b)
    | Five,Zero -> "fifty"
    | Five,b -> "fifty-" + (digitToWord b)
    | Six,Zero -> "sixty"
    | Six,b -> "sixty-" + (digitToWord b)
    | Seven,Zero -> "seventy"
    | Seven,b -> "seventy-" + (digitToWord b)
    | Eight,Zero -> "eighty"
    | Eight,b -> "eighty-" + (digitToWord b)
    | Nine,Zero -> "ninety"
    | Nine,b -> "ninety-" + (digitToWord b)

let rec numberToWord (ds: Digit list) =
    match ds.Length with
    | 0 -> failwith "Can't convert an empty list"
    | 1 -> digitToWord ds.Head
    | 2 -> twoDigitNumberToWord (ds.Head,ds.Tail.Head)
    | 3 -> 
        let (a,b,c) = ds.Item 0,ds.Item 1, ds.Item 2
        match a,b,c with
        | Zero,Zero,Zero -> ""
        | Zero,_,_ -> "and " + (numberToWord ds.Tail)
        | _,Zero,Zero -> (digitToWord ds.Head) + " hundred"
        | _,a,b -> 
            (digitToWord ds.Head) + " hundred and " + (numberToWord ds.Tail)
    | 4 -> (digitToWord ds.Head) + " thousand " + (numberToWord ds.Tail)
    | _ -> failwith "Numbers greater than 9,999 are not supported for this operation."

let euler17 n =
    [1..n]
    |> List.map (numToDigits >> numberToWord)
    |> List.reduce (+)
    |> String.filter (fun c -> c <> ' ' && c <> '-' )
    |> String.length

let solution = euler17 1000
