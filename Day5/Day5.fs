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
let lines = System.IO.File.ReadLines("Day5/test.txt")
                    |> Seq.map (lineInputToLine >> orderLinePoints)
                    |> Seq.toArray

// Strategy:
// Check if segments intersect using orientation https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
// Use Cramer's rule to find point of intersection if not collinear, increment point counter https://www.cuemath.com/geometry/intersection-of-two-lines/
// If collinear, find if overlaps, mark point counters for each overlapping point

let orientation p1 p2 p3 =
//    let slopep1p2 = float (p2.Y - p1.Y)/float (p2.X - p1.X)
//    let slopep2p3 = float (p3.Y - p2.Y)/float (p3.X - p2.X)

    let diff = (p2.Y - p1.Y)*(p3.X - p2.X) - (p2.X - p1.X)*(p3.Y - p2.Y)      
        
    match diff with
    | difference when difference < 0 -> CounterClockwise
    | difference when difference > 0 -> Clockwise
    | difference when difference = 0 -> Collinear
    | _ -> failwith "Unreachable"

let getIntersectionPoint line1 line2 =
    // convert to general form of line: a1x + b1y + c1 = 0
    let a1 = line1.Point1.Y - line1.Point2.Y 
    let b1 = line1.Point2.X - line1.Point1.X
    let c1 = (line1.Point1.X - line1.Point2.X)*line1.Point1.Y +  (line1.Point2.Y - line1.Point1.Y)*line1.Point1.X
    let a2 = line2.Point1.Y - line2.Point2.Y 
    let b2 = line2.Point2.X - line2.Point1.X
    let c2 = (line2.Point1.X - line2.Point2.X)*line2.Point1.Y +  (line2.Point2.Y - line2.Point1.Y)*line2.Point1.X
    
    let x = (b1*c2 - b2*c1)/(a1*b2 - a2*b1)
    let y = (c1*a2 - c2*a1)/(a1*b2 - a2*b1)
    {X = x;  Y = y}
    
let isPointOnLine line point =
    point.X <= Math.Max(line.Point1.X, line.Point2.X) && point.X >= Math.Min(line.Point1.X, line.Point2.X) &&
    point.Y <= Math.Max(line.Point1.Y, line.Point2.Y) && point.Y >= Math.Min(line.Point1.Y, line.Point2.Y)


let getIncrementalPoints startPoint endPoint =
    if startPoint = endPoint then [|startPoint|]
    else
    let slope = float (endPoint.Y - startPoint.Y)/float (endPoint.X - startPoint.X)
    match slope with
    | infinity when Double.IsInfinity infinity -> [|startPoint.Y..endPoint.Y|]
                                                  |> Array.map (fun y -> {X = startPoint.X; Y = y})
    | _ -> [|startPoint.X..endPoint.X|]
            |> Array.indexed
            |> Array.map (fun (i, x) -> {X = x; Y = Convert.ToInt32(float startPoint.Y + (float i)*slope)})    
    

let getOverlappingLinePoints line1 line2 =
    // need to check if WHOLE line is inside other line
    if isPointOnLine line1 line2.Point1 && isPointOnLine line1 line2.Point2
    then Some (getIncrementalPoints line2.Point1 line2.Point2)
    else
    if isPointOnLine line2 line1.Point1 && isPointOnLine line2 line1.Point2
    then Some (getIncrementalPoints line1.Point1 line1.Point2)
    else
    if isPointOnLine line1 line2.Point1
    then Some (getIncrementalPoints line2.Point1 line1.Point2)
    else
    if isPointOnLine line1 line2.Point2
    then Some (getIncrementalPoints line1.Point1 line2.Point2)
    else None    
    
let checkAndGetIntersection (line1,line2) =
    let orientation1 = orientation line1.Point1 line1.Point2 line2.Point1
    let orientation2 = orientation line1.Point1 line1.Point2 line2.Point2
    let orientation3 = orientation line2.Point1 line2.Point2 line1.Point1
    let orientation4 = orientation line2.Point1 line2.Point2 line1.Point2
    
    if orientation1 <> orientation2 && orientation3 <> orientation4
    then Some [|(getIntersectionPoint line1 line2)|]
    else
    if orientation1 = Collinear && orientation2 = Collinear
    then getOverlappingLinePoints line1 line2
    else
    if orientation1 = Collinear && isPointOnLine line1 line2.Point1 then Some [|line2.Point1|]
    else
    if orientation2 = Collinear && isPointOnLine line1 line2.Point2 then Some [|line2.Point2|]
    else
    if orientation3 = Collinear && isPointOnLine line2 line1.Point1 then Some [|line1.Point1|]
    else
    if orientation4 = Collinear && isPointOnLine line2 line1.Point2 then Some [|line1.Point2|]
    else None
            

let horizontalAndVerticalLines = lines |> Array.filter (fun {Point1 = p1; Point2 = p2 } -> p1.X = p2.X || p1.Y = p2.Y)

//let part1 = orientation {X = 0; Y = 0} {X = 4; Y = 4} {X = 1; Y = 2}
let rec customPairing (list:'a list) =
    match list.Length with
    | 0 | 1 -> []
    | 2 -> [(list[0], list[1])]
    | _ -> let pairedList = list.Tail
                             |> List.map (fun el -> (list.Head, el))
           pairedList @ customPairing list.Tail
                        
    
//let result = customPairing [1;2;3;4;5]
let result = customPairing (horizontalAndVerticalLines |> Array.toList)
            |> List.toArray
            |> Array.map (fun pair -> (checkAndGetIntersection pair, pair))
            
//let r = getIncrementalPoints {X = 2; Y = 2} {X = 4; Y = 8}
//let r = getIntersectionPoint {Point1 = {X = 2; Y = 2}; Point2 = {X = 4; Y = 4}} {Point1 = {X = 2; Y = 4}; Point2 = {X = 4; Y = 2}} 
//let r = checkAndGetIntersection ({Point1 = {X = 2; Y = 2}; Point2 = {X = 4; Y = 4}}, {Point1 = {X = 2; Y = 4}; Point2 = {X = 4; Y = 2}}) 
//let r = checkAndGetIntersection ({Point1 = {X = 2; Y = 1}; Point2 = {X = 2; Y = 4}}, {Point1 = {X = 2; Y = 2}; Point2 = {X = 2; Y = 8}}) 
let part1 = result
            