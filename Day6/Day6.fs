module AdventCalendar2021.Day6

open System
open System.Collections.Generic

let line = System.IO.File.ReadLines("Day6/input.txt")
            |> Seq.head
         
let startingFishTimers = line.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Array.map int
let startingFishTimersUint = line.Split(",", StringSplitOptions.RemoveEmptyEntries) |> Array.map uint64

let experienceFishDay fishTimer =
    match fishTimer with
    | 0 -> (6, Some 8)
    | timeLeft -> (timeLeft-1, None)
    
let rec cycleDays daysLeft fishTimers =
    if daysLeft = 0
    then fishTimers
    else
    let oldFishes, nextGenerationFishes = fishTimers
                                        |> Array.map experienceFishDay
                                        |> Array.unzip                                     
    cycleDays (daysLeft-1) (Array.append oldFishes (nextGenerationFishes |> Array.choose id))
    
let part1 = (cycleDays 80 startingFishTimers) |> Array.length
    
let cache = new Dictionary<uint64*uint64, uint64>();
    
let rec countDescendants daysLeft fishTimer =
//    printfn $"DaysLeft: {daysLeft}, FishTimer: {fishTimer}"
    if daysLeft <= fishTimer then 0UL
    else
    let directFishChildrenCount:uint64 = (1UL + (daysLeft - (fishTimer + 1UL))/7UL)
//    printfn $"DirectChildrenCount: {directFishChildrenCount}"
    let firstChildDaysLeft = daysLeft - (fishTimer+1UL)
    ([|0UL..(directFishChildrenCount-1UL)|]
    |> Array.map (fun i ->
            let newDaysLeft = (firstChildDaysLeft - (7UL*i))
            let descendantCount = if cache.ContainsKey (newDaysLeft, 8UL) then cache[(newDaysLeft, 8UL)] else countDescendants newDaysLeft 8UL
            cache[(newDaysLeft, 8UL)] <- descendantCount
            descendantCount)
    |> Array.sum)
    + directFishChildrenCount

let lookupMap = [|0UL..6UL|]
                |> Array.map (fun i -> (i, 1UL + countDescendants 256UL i))
                |> Map.ofArray

let part2 = startingFishTimersUint
            |> Array.map (fun fishTimer -> lookupMap[fishTimer])
            |> Array.sum


// live: 3 daysLeft: 18  (if days > live then spawn: 1 + (days-live+1)/7 else spawn: 0)
// startingSpawnDaysLeft: 18 - (live+1) 
// i in [0..spawnCount]
// live: 8 spawnBirthDaysLeft: startingSpawnDaysLeft - (7*i)
