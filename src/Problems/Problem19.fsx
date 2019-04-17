module Euler.Problem19

(* You are given the following information, but you may prefer to do some
research for yourself.

1 Jan 1900 was a Monday. Thirty days has September, April, June and November.
All the rest have thirty-one, Saving February alone, Which has twenty-eight,
rain or shine. And on leap years, twenty-nine. A leap year occurs on any year
evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1
Jan 1901 to 31 Dec 2000)?*)

type Day =
    | Monday 
    | Tuesday 
    | Wednesday 
    | Thursday 
    | Friday 
    | Saturday 
    | Sunday

let dayLoop = seq { 
    while true do 
        yield! [Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday]
    }

let nextDay d =
    dayLoop
    |> Seq.skipWhile ((<>) d)
    |> Seq.item 1

type MonthName = 
    | January 
    | February 
    | March 
    | April
    | May 
    | June 
    | July 
    | August 
    | September
    | October 
    | November 
    | December

type Month = {
    Name: MonthName
    NumberOfDays: int
    Days: Day list
}

let createMonth name numberOfDays startDay =
    let days =
        dayLoop 
        |> Seq.skipWhile ((<>) startDay)
        |> Seq.take numberOfDays 
        |> Seq.toList
    {
        Name = name
        NumberOfDays = numberOfDays
        Days = days
    }

type YearType = 
    | Normal 
    | LeapYear

type Year = {
    YearNumber: int
    YearType: YearType
    Months: Month list
}

let (|LeapDate|RegularDate|) n =
    if n % 4 = 0 && (n % 100 <> 0 || n % 400 = 0) then
        LeapDate
    else 
        RegularDate

let createYear n startDay =
    let createMonths (l: (MonthName*int) list) =
        l.Tail |> List.fold (fun ml (m,d) -> 
            (createMonth m d (nextDay (List.last ml.Head.Days)))::ml ) 
                [createMonth (fst l.Head) (snd l.Head) startDay]
        |> List.rev
        
    match n with
    | RegularDate -> 
        {
            YearNumber = n
            YearType = Normal
            Months = createMonths [
                        (January,31)
                        (February,28)
                        (March,31)
                        (April,30)
                        (May,31)
                        (June,30)
                        (July,31)
                        (August,31)
                        (September,30)
                        (October,31)
                        (November,30)
                        (December,31)
                    ]
        }
    | LeapDate ->  
        {
            YearNumber = n
            YearType = LeapYear
            Months = createMonths [
                        (January,31)
                        (February,29)
                        (March,31)
                        (April,30)
                        (May,31)
                        (June,30)
                        (July,31)
                        (August,31)
                        (September,30)
                        (October,31)
                        (November,30)
                        (December,31)
                    ]
        }

let lastDayOfYear (y: Year) =
    y.Months
    |> List.last
    |> (fun m -> m.Days |> List.last)

let nextYear (y: Year) =
    createYear (y.YearNumber + 1) ((lastDayOfYear >> nextDay) y)

let yearsSince1900 =
    Seq.unfold (fun y -> Some((nextYear y),(nextYear y))) (createYear 1900 Monday)
    
let solution =
    yearsSince1900
    |> Seq.takeWhile (fun y -> y.YearNumber <= 2000)
    |> Seq.collect (fun y -> y.Months)
    |> Seq.map (fun m -> m.Days.Head)
    |> Seq.sumBy (function | Sunday -> 1 | _ -> 0)
