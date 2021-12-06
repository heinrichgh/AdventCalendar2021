module AdventCalendar2021.Day3

open System

let diagnosticReport = System.IO.File.ReadLines("Day3/input.txt")
                    |> Seq.map (fun elem -> elem |> Seq.map System.Globalization.CharUnicodeInfo.GetDigitValue |> Seq.toArray)
                    |> Seq.toArray
                            
// count 1's into array, compare with list length to see if majority or not
let empty:int[] = Array.zeroCreate diagnosticReport[0].Length

let addTogether totalsList binaries = Array.zip totalsList binaries
                                    |> Array.map (fun (a,b) -> a + b)

let totalOnes = Array.fold addTogether empty diagnosticReport

let isMajority count = count > (diagnosticReport.Length / 2)
let convertBinaryToInteger (string:string) = Convert.ToInt32(string, 2)

let majorityBits = Array.map isMajority totalOnes

let gammaRate = majorityBits
                |> Array.map (fun isMajority -> if isMajority then "1" else "0") 
                |> String.concat ""
                |> convertBinaryToInteger

let epsilonRate = majorityBits
                |> Array.map (fun isMajority -> if isMajority then "0" else "1") 
                |> String.concat ""
                |> convertBinaryToInteger                 

let part1 = gammaRate * epsilonRate

let majorityCheckOxygenGenerator total length = if total >= (length / 2.0) then 1 else 0
let majorityCheckCO2Scrubber total length = if total >= (length / 2.0) then 0 else 1

let rec parseDiagnostics index (majorityChecker:float->float->int) (diagnostics: int[][]) =
    let headBitTotal = diagnostics
                    |> Array.sumBy (fun binaries -> binaries[index])
    let bitToMatch = majorityChecker headBitTotal diagnostics.Length
    let remainingDiagnostics = diagnostics
                            |> Array.filter (fun binaries -> binaries[index] = bitToMatch)
                            
    match remainingDiagnostics.Length with
    | 1 -> remainingDiagnostics
    | _ -> remainingDiagnostics |> parseDiagnostics (index+1) majorityChecker

let oxygenGeneratorRating = parseDiagnostics 0 majorityCheckOxygenGenerator diagnosticReport
                            |> Array.head
                            |> Array.map string
                            |> String.concat ""
                            |> convertBinaryToInteger
                            
let co2ScrubberRating = parseDiagnostics 0 majorityCheckCO2Scrubber diagnosticReport
                            |> Array.head
                            |> Array.map string
                            |> String.concat ""
                            |> convertBinaryToInteger         
let part2 = oxygenGeneratorRating * co2ScrubberRating