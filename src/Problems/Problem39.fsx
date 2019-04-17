module Euler.Problem39

(* If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?*)

let isRightTriangle a b c =
    a * a + b * b = c * c

let rightTrianglesWithPerimeter p = [|
    for c in 1 .. p do
        for a in 1 .. (p - c) / 2 do
            let b = p - (c + a)
            if isRightTriangle a b c then
                yield a, b, c
|]

let solution =
    [| 12 .. 1000 |]
    |> Array.Parallel.map (fun p -> p, rightTrianglesWithPerimeter p)
    |> Array.maxBy (snd >> Array.length)
    |> fst
