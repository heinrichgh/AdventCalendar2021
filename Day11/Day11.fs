module AdventCalendar2021.Day11

type Octopus = {Energy: int; Flashed: bool}
let energyMap = System.IO.File.ReadLines("Day11/input.txt")
                 |> Seq.map (fun line -> line |> Seq.map (fun char -> {Energy = System.Globalization.CharUnicodeInfo.GetDigitValue char; Flashed = false}) |> Seq.toArray) 
                 |> Seq.toArray

let mapWidth = energyMap.Length

let applyToEnergyMap energyMap fn =
    energyMap
    |> Array.map (fun row -> row |> Array.map fn)

let increaseEnergy octopus = if octopus.Flashed then octopus else {octopus with Energy = octopus.Energy + 1}
let setEnergyToZero octopus = {octopus with Energy = 0}
let setFlashed octopus = {octopus with Flashed = true}
    
let increaseEnergyLevels energyMap =
    applyToEnergyMap energyMap increaseEnergy
let resetEnergyMapFlashes energyMap =
    applyToEnergyMap energyMap (fun octopus -> {octopus with Flashed = false})
    
    
let flashOctopus (energyMap:Octopus[][]) row col =    
    if (energyMap[row][col]).Energy <=9 || (energyMap[row][col]).Flashed
    then energyMap
    else    
    let indexUpdateMap =
        [|
            ((row, col), (setEnergyToZero >> setFlashed));
            ((row-1, col), increaseEnergy);
            ((row+1, col), increaseEnergy);
            ((row, col-1), increaseEnergy);
            ((row, col+1), increaseEnergy);
            ((row-1, col+1), increaseEnergy);
            ((row+1, col+1), increaseEnergy);
            ((row-1, col-1), increaseEnergy);
            ((row+1, col-1), increaseEnergy);
        |]
        |> Map.ofArray
    let flashedEnergyMap = energyMap
                            |> Array.mapi (fun rowIdx row -> row
                                                            |> Array.mapi (fun colIdx octopus ->
                                                                                if indexUpdateMap.ContainsKey (rowIdx, colIdx)
                                                                                then indexUpdateMap[(rowIdx, colIdx)] octopus
                                                                                else octopus
                                                                           )
                                           )
    flashedEnergyMap
   
let isThereOutstandingFlashes energyMap =
    (energyMap
    |> Array.filter (fun row -> (row |> Array.filter (fun octopus -> octopus.Flashed = false && octopus.Energy > 9) |> Array.length > 0))
    |> Array.length) > 0        
    
let rec flashEnergyMap energyMap =
    let nextEnergyMap =
       Array.allPairs [|0..mapWidth-1|] [|0..mapWidth-1|]
       |> Array.fold (fun energyMap (row, col) -> flashOctopus energyMap row col) energyMap
    if isThereOutstandingFlashes nextEnergyMap
    then
        flashEnergyMap nextEnergyMap
    else
        nextEnergyMap
    
let rec takeStep stepCount energyMap flashCount=
    if stepCount = 0
    then flashCount
    else
    let increasedEnergyMap = increaseEnergyLevels energyMap
    let flashedEnergyMap = flashEnergyMap increasedEnergyMap 
    let stepFlashCount = flashedEnergyMap
                         |> Array.map (fun row -> row |> Array.filter (fun octopus -> octopus.Flashed) |> Array.length)
                         |> Array.sum
    let nextEnergyMap = resetEnergyMapFlashes flashedEnergyMap
    takeStep (stepCount-1) nextEnergyMap (flashCount+stepFlashCount)
    
let part1 = takeStep 100 energyMap 0

let rec searchForSync stepCount energyMap =    
    let increasedEnergyMap = increaseEnergyLevels energyMap
    let flashedEnergyMap = flashEnergyMap increasedEnergyMap 
    let stepFlashCount = flashedEnergyMap
                         |> Array.map (fun row -> row |> Array.filter (fun octopus -> octopus.Flashed) |> Array.length)
                         |> Array.sum
    if stepFlashCount = mapWidth * mapWidth
    then stepCount
    else
    let nextEnergyMap = resetEnergyMapFlashes flashedEnergyMap
    searchForSync (stepCount+1) nextEnergyMap
    
let part2 = searchForSync 1 energyMap