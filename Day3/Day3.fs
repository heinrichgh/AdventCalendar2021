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

let part1 = gammaRate * epsilonRate;