module AdventCalendar2021.Day7

open System

let line = System.IO.File.ReadLines("Day7/input.txt")
            |> Seq.head
let crabPositions = line.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let uniqueCrabPositions = crabPositions |> Array.distinct

let costPerUniquePosition = uniqueCrabPositions
                            |> Array.map (fun position -> Array.fold (fun cost crabPosition -> cost + Math.Abs (position - crabPosition)) 0 crabPositions)
                            
let part1 = costPerUniquePosition |> Array.min

let costCalculator distance =
    distance * (distance+1) / 2

let minCrabPosition = crabPositions |> Array.min
let maxCrabPosition = crabPositions |> Array.max

let nonConstantCostPerPosition = [|minCrabPosition..maxCrabPosition|]
                                  |> Array.map (fun position -> Array.fold (fun cost crabPosition -> cost + costCalculator (Math.Abs (position - crabPosition)) ) 0 crabPositions)
                                     
let part2 = nonConstantCostPerPosition |> Array.min

// GOLFIFIED
//let crabPositions = (System.IO.File.ReadLines("Day7/input.txt")|> Seq.head).Split(",", StringSplitOptions.RemoveEmptyEntries)|>Array.map int
//let part1 = crabPositions|>Array.distinct|>Array.map (fun position -> Array.fold (fun cost crabPosition -> cost+Math.Abs (position - crabPosition)) 0 crabPositions)|>Array.min
//let part2 = [|(crabPositions|>Array.min)..(crabPositions|>Array.max)|]|>Array.map (fun position -> Array.fold (fun cost crabPosition -> cost + (Math.Abs (position-crabPosition))*((Math.Abs (position-crabPosition))+1)/2 ) 0 crabPositions)|>Array.min                                         