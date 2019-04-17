module Helpers
open System.Collections.Generic

module Optimizations =
    let memoize f =
        let d = Dictionary<_, _>()
        fun x ->
            if d.ContainsKey x then
                d.[x] 
            else 
                let res = f x
                d.[x] <- res
                res

open System
open System.Collections
type Prime =

    static member isPrime(n : Int32) =
        if n = 2 then true
        else if n = 0 || n = 1 || n % 2 = 0 then false
        else
            {3..2..int(sqrt(float n))}
            |> Seq.exists (fun i -> (n % i = 0))
            |> not

    static member isPrime(n : Int64) =
        if n = 2L then true
        else if n = 0L || n = 1L || n % 2L = 0L then false
        else
            {3L..2L..int64(sqrt(float n))}
            |> Seq.exists (fun i -> (n % i = 0L))
            |> not

    static member primesUpTo(n) =
        if n < 2 then []
        else
            let mutable candidates = set ([3 .. 2 .. n])
            let nmax = int (sqrt (float n))
            let mutable primes = set [2]
            let mutable p = 0

            while p < nmax  do
                p <- Set.minElement candidates
                primes <- primes.Add p
                for i in p * p .. 2 * p .. n do
                    candidates <- candidates.Remove i
                candidates <- candidates.Remove p

            Set.toList (primes + candidates)



    static member getPrimes(nmax) =
        let sieve = new BitArray((nmax/2) + 1, true)
        let result = new ResizeArray<int>(nmax / 10)
        let upper = int (sqrt (float nmax))   
        
        if nmax > 1 then result.Add(2) 

        let mutable m = 1
        while 2 * m + 1 <= nmax do
           if sieve.[m] then
               let n = 2 * m + 1
               if n <= upper then 
                   let mutable i = m
                   while 2 * i < nmax do sieve.[i] <- false; i <- i + n
               result.Add n
           m <- m + 1
        
        result



    // Incorrect implementation 
    // let largestPrimeFactor n =
    //     {(n / int64(sqrt(float n)))..(-1L)..2L}
    //     |> Seq.find (fun i -> n % i = 0L && (isPrime i))

    // let primeRange n =
    //     let n' = float n
    //     let lower =  int64 (floor (n' * (log(n') + log(log(n')) - 1.0)))
    //     let upper =  int64 (ceil (n' * (log(n') + log(log(n')))))
    //     [lower..upper]

module Permutations =
    let rec private inserts x l =
      seq { match l with
            | [] -> yield [x]
            | y::rest ->
                yield x::l
                for i in inserts x rest do
                  yield y::i
          }

    let rec ofList l =
      seq { match l with
            | [] -> yield []
            | x::rest ->
                for p in ofList rest do
                  yield! inserts x p
          }
          