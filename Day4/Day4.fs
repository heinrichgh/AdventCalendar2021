module AdventCalendar2021.Day4

open System

let input =
    System.IO.File.ReadAllText("Day4/input.txt")

let newLine = "\r\n"
let sections = input.Split(newLine + newLine)

let drawnNumbers =
    sections[0].Split(",", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int

let bingoBoards =
    Array.skip 1 sections
    |> Array.map
        (fun line ->
            line.Split([| newLine; " " |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (int >> Some))


let rec markNumber number bingoBoard =
    bingoBoard
    |> Array.map (fun x -> if x = Some(number) then None else x)

let rowsToColumns rows =
    rows
    |> Array.collect Array.indexed
    |> Array.groupBy fst
    |> Array.map (snd >> Array.map snd)
    
let checkAllMarked numbers =
    let unmarked = numbers |> Array.choose id |> Array.length
    unmarked = 0
    
let checkForWinner bingoBoard =
    let rows = bingoBoard |> Array.chunkBySize 5
    let finishedRows = rows |> Array.filter checkAllMarked
    match finishedRows.Length with
    | 0 ->     
        let columns = rowsToColumns rows
        let finishedColumns = columns |> Array.filter checkAllMarked
        finishedColumns.Length <> 0
    | _ -> true

let calculateFinalScore (board: int option []) number =
    let boardScore = board |> Array.choose id |> Array.sum
    boardScore * number

let rec playBingo (numbers: int []) bingoBoards =
    let number = Array.head numbers

    let boards =
        bingoBoards |> Array.map (markNumber number)

    let winnerBoard = boards |> Array.tryFind checkForWinner

    match winnerBoard with
    | Some (board) -> (board, number)
    | None -> playBingo (Array.tail numbers) boards

let (winningBoard, lastNumber) = playBingo drawnNumbers bingoBoards
let sumOfUnmarkedNumbers = winningBoard |> Array.choose id |> Array.sum
let part1 = sumOfUnmarkedNumbers * lastNumber

let rec playLoserBingo (numbers: int list) bingoBoards currentWinner currentWinningNumber =   
    match numbers with
    | number::tail ->
        let boards =
            bingoBoards |> Array.map (markNumber number)

        let winnerBoard = boards |> Array.tryFind checkForWinner
        match winnerBoard with
        | Some(board) -> playLoserBingo tail (boards |> Array.filter (checkForWinner >> not)) board number
        | None -> playLoserBingo tail boards currentWinner currentWinningNumber
    | _ -> (currentWinner, currentWinningNumber)

let (winningLoserBoard, lastLoserNumber) = playLoserBingo (drawnNumbers |> Array.toList) bingoBoards [||] 0
let sumOfUnmarkedLoserNumbers = winningLoserBoard |> Array.choose id |> Array.sum
let part2 = sumOfUnmarkedLoserNumbers * lastLoserNumber