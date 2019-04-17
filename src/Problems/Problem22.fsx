module Euler.Problem22

(* Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
containing over five-thousand first names, begin by sorting it into alphabetical
order. Then working out the alphabetical value for each name, multiply this
value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?*)

open System.IO

let dataString =
    use sr = new StreamReader(__SOURCE_DIRECTORY__ + "/Euler22.txt")
    sr.ReadToEnd()

let data = dataString.Replace("\"","").Split([|','|]) |> Array.toList

let alphaValue (s: string) =
    s.ToCharArray()
    |> Array.toList
    |> List.sumBy (int >> ((+) -64))

let euler22 data =
    data
    |> List.sort
    |> List.mapi (fun i s -> (i + 1) * (alphaValue s))
    |> List.sum

let solution = euler22 data
