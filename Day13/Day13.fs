module AdventCalendar2021.Day13

open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText("Day13/input.txt")

let splitInput = input.Split("\r\n\r\n", StringSplitOptions.RemoveEmptyEntries);
let dots = (splitInput[0]).Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
           |> Array.map (fun line ->
               let split = line.Split(",", StringSplitOptions.RemoveEmptyEntries)
               (int split[0], int split[1])               
               )
        
let folds = (splitInput[1]).Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun line ->
                let regexMatch = Regex.Matches(line, "fold along (x|y)=([0-9]+)")                
                                 |> Seq.head
                (regexMatch.Groups[1].ToString(), int (regexMatch.Groups[2].ToString()))                
                )

let foldX coordinate (dots:(int*int)[]) =
    dots
    |> Array.map (fun (x, y) ->
        let newX = if x > coordinate then coordinate - (x-coordinate) else x
        (newX, y))
    |> Array.distinct
    
let foldY coordinate (dots:(int*int)[]) =    
    dots
    |> Array.map (fun (x, y) ->
        let newY = if y > coordinate then coordinate - (y-coordinate) else y
        (x, newY))
    |> Array.distinct

let doFold fold (dots:(int*int)[]) =
    if fst fold = "x"
    then foldX (snd fold) dots
    else foldY (snd fold) dots

let part1 = doFold folds[0] dots |> Array.length
       

let rec parseFolds folds dots =
    match folds with
    | head::tail -> parseFolds tail (doFold head dots)
    | _ -> dots

let createDrawing dots =
    let sizeX = (dots
                |> Array.map fst
                |> Array.max) + 1
    let sizeY = (dots
                |> Array.map snd
                |> Array.max) + 1
    
    let partialDrawing = (Array.zeroCreate sizeY)
                        |> Array.mapi (fun y _ ->
                                        (Array.zeroCreate sizeX)
                                        |> Array.mapi (fun x _ -> if Array.contains (x,y) dots then "#" else " ")
                                       )
                        |> Array.map (fun row -> String.Join("", row))
    
    "\r\n" + String.Join("\r\n", partialDrawing) + "\r\n"
    
let part2 = createDrawing (parseFolds (folds |> Array.toList) dots)
