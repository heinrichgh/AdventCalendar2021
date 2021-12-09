module AdventCalendar2021.Day9

open System

let heightMap = System.IO.File.ReadLines("Day9/input.txt")
                 |> Seq.map (fun line -> line |> Seq.map System.Globalization.CharUnicodeInfo.GetDigitValue |> Seq.toArray) 
                 |> Seq.toArray

let mapWidth = heightMap[0].Length
let mapLength = heightMap.Length

let isLowestPoint row col =
    let height = heightMap[row][col]
    if row = 0
    then
        if col = 0
        then
            height < heightMap[row][col+1]
            && height < heightMap[row+1][col]
        else
        if col = mapWidth-1
        then
            height < heightMap[row][col-1]
            && height < heightMap[row+1][col]
        else
            height < heightMap[row][col+1]
            && height < heightMap[row][col-1]
            && height < heightMap[row+1][col]
    else
    if row = mapLength-1
    then
        if col = 0
        then
            height < heightMap[row][col+1]
            && height < heightMap[row-1][col]
        else
        if col = mapWidth-1
        then
            height < heightMap[row][col-1]
            && height < heightMap[row-1][col]
        else
            height < heightMap[row][col+1]
            && height < heightMap[row][col-1]
            && height < heightMap[row-1][col]
    else
    if col = 0
    then  
        height < heightMap[row][col+1]
        && height < heightMap[row-1][col]
        && height < heightMap[row+1][col]
    else    
    if col = mapWidth-1
    then
        height < heightMap[row][col-1]
        && height < heightMap[row-1][col]
        && height < heightMap[row+1][col]
    else
        height < heightMap[row][col+1]
            && height < heightMap[row][col-1]
            && height < heightMap[row+1][col]
            && height < heightMap[row-1][col]
        
let part1 = Array.allPairs [|0..mapLength-1|] [|0..mapWidth-1|]
            |> Array.filter (fun (row, col) -> isLowestPoint row col)
            |> Array.map (fun (row, col) -> heightMap[row][col] + 1)
            |> Array.sum

let rec testCreep row col =
    if heightMap[row][col] <> 9
    then getBasins row col
    else [None]
and getBasins row col =
    heightMap[row][col] <- 9
    [Some (row, col)]
    @
    if row = 0
    then
        if col = 0
        then
            testCreep row (col+1)
            @ testCreep (row+1) col
        else
        if col = mapWidth-1
        then            
            testCreep row (col-1)
            @ testCreep (row+1) col
        else
            testCreep row (col+1)
            @ testCreep row (col-1)
            @ testCreep (row+1) col
    else
    if row = mapLength-1
    then
        if col = 0
        then
            testCreep row (col+1)
            @ testCreep (row-1) col
        else
        if col = mapWidth-1
        then
            testCreep row (col-1)
            @ testCreep (row-1) col
        else
            testCreep row (col+1)
            @ testCreep row (col-1)
            @ testCreep (row-1) col
    else
    if col = 0
    then  
        testCreep row (col+1)
        @ testCreep (row-1) col
        @ testCreep (row+1) col
    else    
    if col = mapWidth-1
    then
        testCreep row (col-1)
        @ testCreep (row-1) col
        @ testCreep (row+1) col
    else
        testCreep row (col+1)
        @ testCreep row (col-1)
        @ testCreep (row-1) col
        @ testCreep (row+1) col
    
            
let lowPoints = Array.allPairs [|0..mapLength-1|] [|0..mapWidth-1|]
                |> Array.filter (fun (row, col) -> isLowestPoint row col)  
                |> Array.map ((fun (row, col) -> getBasins row col)
                              >> List.choose id
                              >> List.distinct
                              >> List.length
                              )
                |> Array.sortDescending
                |> Array.take 3
                |> Array.fold (fun accu value -> accu * value ) 1
                
let part2 = lowPoints

                
            