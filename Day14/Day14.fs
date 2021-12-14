module AdventCalendar2021.Day14

open System.Text.RegularExpressions

let input = System.IO.File.ReadLines("Day14/input.txt") |> Seq.toList

let template = input.Head

let rules = List.skip 2 input
            |> List.map (fun line ->
                                let regexMatch = Regex.Matches(line, "([A-Z])([A-Z]) -> ([A-Z])")                
                                                |> Seq.head
                                ((regexMatch.Groups[1].ToString()[0], regexMatch.Groups[2].ToString()[0]), regexMatch.Groups[3].ToString())                                
                        )
            |> Map.ofList

let rec runPolymerInsertionCycle stepCount (template:string) =
    printfn "%A" (stepCount, template.Length)
    if stepCount = 0
    then template
    else
        let nextTemplate = (Array.pairwise (template |> Seq.toArray)
                         |> Array.map (fun (a,b) ->
                                        if rules.ContainsKey (a,b)
                                        then a.ToString() + rules[(a,b)]
                                        else a.ToString()
                             )
                         |> Array.fold (fun acc curr -> acc + curr) "") + (template[template.Length-1].ToString())                          
        runPolymerInsertionCycle (stepCount-1) nextTemplate
        

let polymerResult = runPolymerInsertionCycle 10 template
let polymerCounts = polymerResult
                             |> Seq.groupBy id
                             |> Seq.map (fun (_, charList) -> Seq.length charList)
                             

let part1 = (Seq.max polymerCounts) - (Seq.min polymerCounts)


let polymerResultPart2 = runPolymerInsertionCycle 40 template
let polymerCountsPart2 = polymerResult
                             |> Seq.groupBy id
                             |> Seq.map (fun (_, charList) -> Seq.length charList)
                             

let part2 = (Seq.max polymerCounts) - (Seq.min polymerCounts)