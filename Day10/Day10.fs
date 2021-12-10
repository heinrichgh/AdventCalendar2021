module AdventCalendar2021.Day10

open System
open AdventCalendar2021.Day5

type OkResult = Complete | Incomplete

let lines = System.IO.File.ReadLines("Day10/input.txt")
            |> Seq.map Seq.toList
            |> Seq.toArray
            
let chunkPairs = [
                    ('(', ')');
                    ('[', ']');
                    ('{', '}');
                    ('<', '>');
                 ]
                |> Map.ofList
    
let scoreLookup = [
                    (')', 3)
                    (']', 57)
                    ('}', 1197)
                    ('>', 25137)
                  ]        
                  |> Map.ofList
let openingCharacters = chunkPairs.Keys |> Set.ofSeq
let closingCharacters = chunkPairs.Values |> Set.ofSeq

let isOpeningCharacter c = Set.contains c openingCharacters

let rec findCorruptedCharacter line (currentOpenCharacters:char list) =    
    if List.isEmpty line && List.isEmpty currentOpenCharacters
    then Ok Complete
    else
    if List.isEmpty currentOpenCharacters
    then Ok Incomplete
    else
    let currentOpenCharacter = currentOpenCharacters.Head
    if not (isOpeningCharacter currentOpenCharacter)
    then Error currentOpenCharacter
    else
    match line with
    | head::tail -> if isOpeningCharacter head
                    then                        
                        findCorruptedCharacter tail (head::currentOpenCharacters)                        
                    else                    
                    if chunkPairs[currentOpenCharacter] = head
                    then findCorruptedCharacter tail currentOpenCharacters.Tail
                    else Error head
    | _ -> Ok Incomplete
    
let startFindingCorruptedCharacter (line:char list) =
    findCorruptedCharacter line.Tail (List.take 1 line)
                    
//let part1 = startFindingCorruptedCharacter ("]]()" |> Seq.toList)                     
//let part1 = startFindingCorruptedCharacter ("([])" |> Seq.toList)                   

let part1 = lines
            |> Array.map (fun line -> startFindingCorruptedCharacter line)
            |> Array.filter (fun result -> match result with
                                            | Ok _ -> false
                                            | Error _ -> true)
            |> Array.map ((fun result -> match result with
                                            | Error x -> x
                                            | _ -> raise (Exception("Filter did not filter properly")))
                         >> (fun corruptedBrace -> scoreLookup[corruptedBrace]))
            |> Array.sum
            
