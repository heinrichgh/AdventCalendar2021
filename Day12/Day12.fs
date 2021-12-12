module AdventCalendar2021.Day12

open System

type Node = {Label: string; LargeCave: bool}

let lineToNodePair (line:string) =
    let split = line.Split("-", StringSplitOptions.RemoveEmptyEntries)
    let node0 = {Label = split[0]; LargeCave = split[0].ToUpperInvariant() = split[0]}
    let node1 = {Label = split[1]; LargeCave = split[1].ToUpperInvariant() = split[1]}
    [|(node0, node1); (node1, node0)|]
    
let nodeMap = System.IO.File.ReadLines("Day12/input.txt")
            |> Seq.map lineToNodePair
            |> Seq.collect id
            |> Seq.groupBy fst
            |> Seq.map (fun (node, nodeList) -> (node, nodeList |> Seq.map snd |> Set.ofSeq))
            |> Map.ofSeq
            
let startingNode = {Label = "start"; LargeCave = false}           
let endNode = {Label = "end"; LargeCave = false}           
let visitedNodes = [|startingNode|] |> Set.ofArray

let rec getPathsThroughCave currentNode visitedNodes currentNodePath =
    let unvisitedConnectedNodes = Set.difference nodeMap[currentNode] visitedNodes
    if unvisitedConnectedNodes.IsEmpty
    then [None]
    else
    let updatedVisitedNodes = if currentNode.LargeCave then visitedNodes else visitedNodes.Add currentNode
    let updatedNodePath = currentNode::currentNodePath
    if currentNode = endNode
    then [Some updatedNodePath]
    else 
    unvisitedConnectedNodes
    |> Set.toList
    |> List.map (fun node -> getPathsThroughCave node updatedVisitedNodes updatedNodePath)
    |> List.collect id
             
let validPart1Paths = getPathsThroughCave startingNode visitedNodes []
                      |> List.choose id
let part1 = validPart1Paths |> List.length