module AdventCalendar2021.Day8

open System

let lines = System.IO.File.ReadLines("Day8/input.txt")
                    |> Seq.toArray
                    
let fourDigitSegments = lines |> Array.map (fun line -> ((line.Split "|")[1]).Split(" ", StringSplitOptions.RemoveEmptyEntries))

let isKnownDigitSegmentLength length = length = 2 || length = 3 || length = 4 || length = 7

let part1 = fourDigitSegments
            |> Array.map (fun digits -> digits |> Array.filter (fun digit -> isKnownDigitSegmentLength digit.Length) |> Array.length )
            |> Array.sum
            
type Display = {SignalPattern: Set<char>[]; FourDigitSignalValue: Set<char>[]}

let convertToSetOfCharacters (signalPatternString:string) =
        signalPatternString.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun string -> string.ToCharArray() |> Set.ofArray)

let displays = lines
               |> Array.map (fun line ->
                                   let splitLine = (line.Split "|")
                                   { SignalPattern = convertToSetOfCharacters splitLine[0]; FourDigitSignalValue = convertToSetOfCharacters splitLine[1]})
   
let sevenSegments (signalPatterns:Set<char>[]) =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 3) |> Array.head
    
let oneSegments (signalPatterns:Set<char>[]) =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 2) |> Array.head
    
let fourSegments (signalPatterns:Set<char>[]) =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 4) |> Array.head
    
let eightSegments (signalPatterns:Set<char>[]) =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 7) |> Array.head
      
let nineSegments (signalPatterns:Set<char>[]) sevenSegments fourSegments eightSegments=
    let sevenFourUnion = Set.union sevenSegments fourSegments
    signalPatterns |> Array.filter (fun pattern -> pattern <> eightSegments && Set.isProperSuperset pattern sevenFourUnion) |> Array.head

let fiveSegments (signalPatterns:Set<char>[]) fourSegments oneSegments =
    let fourOneDiff = Set.difference fourSegments oneSegments
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 5 && Set.isProperSuperset pattern fourOneDiff) |> Array.head

let sixSegments (signalPatterns:Set<char>[]) fourSegments oneSegments nineSegments =
    let fourOneDiff = Set.difference fourSegments oneSegments
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 6 && pattern <> nineSegments && Set.isProperSuperset pattern fourOneDiff) |> Array.head
    
let zeroSegments (signalPatterns:Set<char>[]) sixSegments nineSegments =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 6 && pattern <> sixSegments && pattern <> nineSegments) |> Array.head
    
let twoSegments (signalPatterns:Set<char>[]) fiveSegments oneSegments =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 5 && pattern <> fiveSegments && Set.count (Set.difference pattern oneSegments) = 4 ) |> Array.head
    
let threeSegments (signalPatterns:Set<char>[]) fiveSegments twoSegments =
    signalPatterns |> Array.filter (fun pattern -> Set.count pattern = 5 && pattern <> fiveSegments && pattern <> twoSegments) |> Array.head
            
let decodeDisplay display =
    let one = (oneSegments display.SignalPattern, 1)
    let four = (fourSegments display.SignalPattern, 4)
    let seven = (sevenSegments display.SignalPattern, 7)
    let eight = (eightSegments display.SignalPattern, 8)
    let nine = (nineSegments display.SignalPattern (fst seven) (fst four) (fst eight), 9)
    let five = (fiveSegments display.SignalPattern (fst four) (fst one), 5)
    let six = (sixSegments display.SignalPattern (fst four) (fst one) (fst nine), 6)
    let zero = (zeroSegments display.SignalPattern (fst six) (fst nine), 0)
    let two = (twoSegments display.SignalPattern (fst five) (fst one), 2)
    let three = (threeSegments display.SignalPattern (fst five) (fst two), 3)

    let lookup = [|one; four; seven; eight; nine; five; six; zero; two; three|]
                |> Map.ofArray
                
    String.Join("", display.FourDigitSignalValue |> Array.map (fun set -> lookup[set])) |> int
            
let part2 = displays
            |> Array.map decodeDisplay
            |> Array.sum

// GOLFIFIED
//let part1 = System.IO.File.ReadLines("Day8/input.txt")|>Seq.toArray|>Array.map ((fun line -> ((line.Split "|")[1]).Split(" ", StringSplitOptions.RemoveEmptyEntries))>>(fun digits -> digits|>Array.filter (fun digit -> Array.contains digit.Length [|2;3;4;7|])|>Array.length ))|>Array.sum
