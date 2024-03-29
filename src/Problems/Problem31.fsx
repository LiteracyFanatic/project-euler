module Euler.Problem31

(* In England the currency is made up of pound, £, and pence, p, and there are
eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p). It is possible to make £2 in
the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p How many different ways can £2 be made
using any number of coins?*)

let coinValues = [200; 100; 50; 20; 10; 5; 2; 1]

let fillWallet coins fillValue =
    let rec loop count wallet coins =
        if List.sum wallet = fillValue
        then 
            wallet :: count
        else
            coins
            |> List.filter (fun coin -> coin + List.sum wallet <= fillValue)
            |> List.collect (fun coin ->
                let newCoins = List.filter (fun c -> c <= coin) coins
                loop count (coin :: wallet) newCoins
            )            
    loop [] [] coins

let solution =
    fillWallet coinValues 200
    |> List.length
    