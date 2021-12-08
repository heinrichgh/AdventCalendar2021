module AdventCalendar2021.Day5

open System

type Point = {X: int; Y:int}
type Line = {Point1: Point; Point2: Point}
type Orientation =
    | Clockwise
    | CounterClockwise
    | Collinear

let orderLinePoints line =
    if line.Point1.X > line.Point2.X
    then {Point1 = line.Point2; Point2 = line.Point1}
    else line

let lineInputToLine (line:string) =
    let lineParts = line.Split(" -> ", StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun linePart -> linePart.Split "," |> Array.map int)       
    {Point1 = {X = lineParts[0][0]; Y = lineParts[0][1]}; Point2 = {X = lineParts[1][0]; Y = lineParts[1][1]}}
let lines = System.IO.File.ReadLines("Day5/input.txt")
                    |> Seq.map (lineInputToLine >> orderLinePoints)
                    |> Seq.toArray
                    
let horizontalAndVerticalLines = lines |> Array.filter (fun {Point1 = p1; Point2 = p2 } -> p1.X = p2.X || p1.Y = p2.Y)
                    
let getIncrementalPoints startPoint endPoint =
    if startPoint = endPoint then [|startPoint|]
    else    
    match endPoint.X - startPoint.X with
    | 0 -> [|Math.Min(startPoint.Y, endPoint.Y)..Math.Max(startPoint.Y, endPoint.Y)|]
            |> Array.map (fun y -> {X = startPoint.X; Y = y})
    | divisor -> let slope = (endPoint.Y - startPoint.Y)/divisor
                 [|startPoint.X..endPoint.X|]
                    |> Array.indexed
                    |> Array.map (fun (i, x) -> {X = x; Y = startPoint.Y + i*slope})
                    
let horizontalAndVerticalPoints = horizontalAndVerticalLines
                                 |> Array.map (fun {Point1 = startPoint; Point2 = endPoint} -> getIncrementalPoints startPoint endPoint)
                                 |> Array.collect id
             
let horizontalAndVerticalGroupedPoints = horizontalAndVerticalPoints
                                        |> Array.groupBy id
                    
let part1 = horizontalAndVerticalGroupedPoints
            |> Array.filter (fun (_, list) -> list.Length > 1)
            |> Array.length
            
let points = lines
             |> Array.map (fun {Point1 = startPoint; Point2 = endPoint} -> getIncrementalPoints startPoint endPoint)
             |> Array.collect id
             
let groupedPoints = points
                    |> Array.groupBy id
                    
let part2 = groupedPoints
            |> Array.filter (fun (_, list) -> list.Length > 1)
            |> Array.length