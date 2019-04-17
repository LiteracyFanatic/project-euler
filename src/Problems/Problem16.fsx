module Euler.Problem16

(* What is the sum of the digits of the number 2^1000?*)

open System

let carryOver l =
    let rec innerCarryOver l acc =
        match l with
        | [] -> acc
        | h::t ->
            if h > 9 then
                innerCarryOver (t.Head + (h / 10)::t.Tail) (h % 10::acc)
            else 
                innerCarryOver t (h::acc)
    innerCarryOver (List.rev (0::l)) []

let addLists (l1: int list) (l2: int list) =
    let resizeLists (l1: int list) (l2: int list) =
        match (l1.Length - l2.Length) with
        | n when n >= 0 -> l1,(List.init n (fun i -> 0))@l2
        | n when n < 0 -> (List.init -n (fun i -> 0))@l1,l2
    let (l1',l2') = resizeLists l1 l2
    List.map2 (+) l1' l2'

let multLists (l1: int list) (l2: int list) =
    let reorderLists (l1: int list) (l2: int list) =
        match (l1.Length - l2.Length) with
        | n when n >= 0 -> l1,l2
        | n when n < 0 -> l2,l1
    let (l1',l2') = reorderLists l1 l2
    l2'
    |> List.map (fun i -> List.map (fun x -> x * i) l1')
    |> List.rev
    |> List.mapi (fun n l -> l @ List.init n (fun i -> 0) )
    |> List.reduce (fun i -> (addLists i))
    |> carryOver
    |> List.skipWhile ((=) 0)

let numToList n =
    [for c in n.ToString() -> c.ToString()]
    |> List.map Int32.Parse

let bigPow x n =
    let x' = numToList x
    List.init n (fun i -> x')
    |> List.reduce multLists

let euler16 x n = (bigPow x n) |> List.sum

let solution = euler16 2 1000
