module AdventCalendar2021.Day1

let numberList = System.IO.File.ReadLines("Day1/input.txt")
                 |> Seq.map int
                 |> Seq.toList

let countIncreases = fun acc elem ->
    match acc with
    | (prev, total) when prev < elem -> (elem, total+1)
    | (_, total) -> (elem, total)
                 
let part1 = snd (List.fold countIncreases (numberList.Head, 0) numberList.Tail)

let calculateIncreases inputList = List.pairwise inputList
                                 |> List.filter (fun (a, b) -> b > a)
                                 |> List.length                 
let part1again = calculateIncreases numberList // learned about List.pairwise during part2                 
let listsTargetLength = numberList.Length - 2;

let numberListTruncated = List.truncate listsTargetLength numberList
let numberListShiftedOnce = List.truncate listsTargetLength numberList.Tail
let numberListShiftedTwice = List.truncate listsTargetLength numberList.Tail.Tail

let part2 = List.zip3 numberListTruncated numberListShiftedOnce numberListShiftedTwice
            |> List.map (fun (a, b, c) -> a + b + c)
            |> calculateIncreases
