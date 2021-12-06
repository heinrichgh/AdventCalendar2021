module AdventCalendar2021.Day3Part2

open System

let diagnosticReport = System.IO.File.ReadLines("Day3/test.txt")
                    |> Seq.map (fun elem -> elem |> Seq.map System.Globalization.CharUnicodeInfo.GetDigitValue |> Seq.toArray)
                    |> Seq.toArray
                            
let majorityCheckOxygenGenerator total length = if total >= (length / 2.0) then 1 else 0
let majorityCheckCO2Scrubber total length = if total >= (length / 2.0) then 0 else 1

let rec parseDiagnostics index (majorityChecker:float->float->int) (diagnostics: int[][]) =
//    printfn $"Current Diagnostics: %A{diagnostics}"
    let headBitTotal = diagnostics
                    |> Array.sumBy (fun binaries -> binaries[index])
    let bitToMatch = majorityChecker headBitTotal diagnostics.Length
//    printfn $"Bit to match: %A{bitToMatch}"
    let remainingDiagnostics = diagnostics
                            |> Array.filter (fun binaries -> binaries[index] = bitToMatch)
                            
//    printfn $"Remaining Diagnostics: %A{remainingDiagnostics}"
    match remainingDiagnostics.Length with
    | 1 -> remainingDiagnostics
    | _ -> remainingDiagnostics |> parseDiagnostics (index+1) majorityChecker

let oxygenGeneratorRating = parseDiagnostics 0 majorityCheckOxygenGenerator diagnosticReport
let part2 = oxygenGeneratorRating