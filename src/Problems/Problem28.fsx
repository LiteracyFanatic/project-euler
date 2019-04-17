module Euler.Problem28

(* Starting with the number 1 and moving to the right in a clockwise direction a
5 by 5 spiral is formed as follows:

21 22 23 24 25 20  7  8  9 10 19  6  1  2 11 18  5  4  3 12 17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
in the same way?*)

//    let getCorners (ring: int list) =
//        match ring.Length with
//        | 0 -> [0]
//        | 1 -> ring
//        | _ ->
//            let offset = ((ring.Length / 4) - 1)
//            List.init 4 (fun i -> offset + i * ring.Length / 4)
//            |> List.map (fun i -> ring.Item i)
//
//    let splitAt (inds: int list) (l: int list) =
//        let rec innerSplitAt (inds: int list) (l: int list) (acc: int list list) =
//            match inds with
//            | [] -> l::acc
//            | h::t ->
//                let acc',l' = List.splitAt h l
//                innerSplitAt t l' (acc'::acc)
//        let newInds = 
//            inds
//            |> List.scan (fun (a,b) t -> t - b,t) (0,0)
//            |> List.map fst
//            |> List.tail
//        innerSplitAt newInds l [[]]
//        |> List.rev
//        |> List.tail
//
//    let ringLengths =
//            Seq.initInfinite (fun i -> (i+1) * 4 - 4)
//            |> Seq.skip 2
//            |> Seq.mapi (fun i e -> if i % 2 = 0 then Some(e) else None)
//            |> Seq.choose id
//            |> Seq.append [1]
//
//    let splitInds =
//        ringLengths
//        |> Seq.scan (+) 0
//        |> Seq.skip 1
//        |> Seq.takeWhile ((>) (1001*1001))
//        |> Seq.toList
//
//    [1..1001*1001]
//    |> splitAt splitInds
//    |> List.collect getCorners
//    |> List.sum

// LOL, looks like math trumps brute force on this one :P
let euler28 n =
    [3..2..n]
    |> List.sumBy (fun n -> 4*n*n - 6*n + 6)
    |> (+) 1

let solution = euler28 1001
