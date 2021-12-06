module AdventCalendar2021.Day2

type Position = {Horizontal: int; Depth: int}
type Instruction = {Command: string; Magnitude: int}

let arrayToInstruction (array:string[]) = {Command = array[0]; Magnitude = int array[1]}
let instructions = System.IO.File.ReadLines("Day2/input.txt")
                |> Seq.map (fun line -> arrayToInstruction (line.Split " "))
                |> Seq.toList

let startingPoint = {Horizontal = 0; Depth = 0}

let handleInstruction currentPosition (instruction:Instruction) =
    match instruction with
    | {Command = "forward"; Magnitude = magnitude} -> {currentPosition with Horizontal = currentPosition.Horizontal + magnitude}
    | {Command = "up"; Magnitude = magnitude} -> {currentPosition with Depth = currentPosition.Depth - magnitude}
    | {Command = "down"; Magnitude = magnitude} -> {currentPosition with Depth = currentPosition.Depth + magnitude}
    | _ -> failwith "Unknown command"

let finishPoint = List.fold handleInstruction startingPoint instructions
let part1 = finishPoint.Horizontal * finishPoint.Depth


