module AdventCalendar2021.Day14

open System.Collections.Generic
open System.Text.RegularExpressions

let input = System.IO.File.ReadLines("Day14/test.txt") |> Seq.toList

let template = input.Head

let rules = List.skip 2 input
            |> List.map (fun line ->
                                let regexMatch = Regex.Matches(line, "([A-Z])([A-Z]) -> ([A-Z])")                
                                                |> Seq.head
                                ((regexMatch.Groups[1].ToString()[0], regexMatch.Groups[2].ToString()[0]), regexMatch.Groups[3].ToString())                                
                        )
            |> Map.ofList

let rec runPolymerInsertionCycle stepCount (template:string) =
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

let rulesForFaster = List.skip 2 input
                    |> List.map (fun line ->
                                        let regexMatch = Regex.Matches(line, "([A-Z])([A-Z]) -> ([A-Z])")                
                                                        |> Seq.head
                                        ((regexMatch.Groups[1].ToString(), regexMatch.Groups[2].ToString()), regexMatch.Groups[3].ToString())                                
                                )
                    |> Map.ofList

let pairExpandCache = new Dictionary<string*string*int, string>();

let rec expandPair stepCount (a,b) endPiece =
    if stepCount = 0
    then
        if endPiece then b else a+b
//        a+b
    else
    if pairExpandCache.ContainsKey (a, b, stepCount)
    then pairExpandCache[(a, b, stepCount)]
    else
    if rulesForFaster.ContainsKey (a,b)
    then
        let insertion = rulesForFaster[(a,b)]
        let result = (expandPair (stepCount - 1) (a, insertion) false) + (expandPair (stepCount - 1) (insertion, b) true)
        pairExpandCache[(a, b, stepCount)] <- result
        result
    else
        if endPiece then b else a+b
//        a+b
    
let runPolymerInsertionCycleFaster stepCount (template:string[]) =   
    Array.pairwise template
     |> Array.map (fun pair ->
                    expandPair stepCount pair false
         )
     |> Array.fold (fun acc curr -> acc + curr) ""
         
        
let polymerResultPart2 = runPolymerInsertionCycleFaster 2 (template |> Seq.toArray |> Array.map (fun c -> c.ToString()))
let polymerCountsPart2 = polymerResult
                             |> Seq.groupBy id
                             |> Seq.map (fun (_, charList) -> Seq.length charList)
                             

let part2 = polymerResultPart2