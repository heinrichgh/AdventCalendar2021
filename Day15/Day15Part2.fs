module AdventCalendar2021.Day15Part2

open System

[<CustomComparison; CustomEquality>]
type Vertex =
    {CostOfEntry: uint; Distance: uint; EstimatedDistance:float; Id:int*int}
    
    interface System.IComparable with
        member x.CompareTo yObj =
            match yObj with
            | :? Vertex as y ->
                if (fst x.Id = fst y.Id) && (snd x.Id = snd y.Id)
                then 0
                else
                let guessedDistanceX = (float x.Distance + x.EstimatedDistance)
                let guessedDistanceY = (float y.Distance + y.EstimatedDistance)
                if (guessedDistanceX = guessedDistanceY) then -1 else guessedDistanceX.CompareTo guessedDistanceY
            | _ -> invalidArg "yObj" "can only compare Vertex with other Vertex"
    override x.GetHashCode() = hash x            
    override x.Equals(yObj) =
        match yObj with
        | :? Vertex as y -> x.Id = y.Id
        | _ -> false

let lineToVertices lineIndex (line:string) =
    line
    |> Seq.mapi (fun colIndex char ->
        let cost = if lineIndex = 0 && colIndex = 0 then 0u else uint (System.Globalization.CharUnicodeInfo.GetDigitValue char)
        {Id = (colIndex, lineIndex)
         CostOfEntry = cost
         Distance = if lineIndex = 0 && colIndex = 0 then 0u else uint Int32.MaxValue
         EstimatedDistance = Math.Sqrt ((99.0-float colIndex)*(99.0-float colIndex) + (99.0-float lineIndex)*(99.0-float lineIndex))
         }
        )

let verticesGrid = System.IO.File.ReadLines("Day15/input.txt")
                |> Seq.mapi lineToVertices

let maxX = (Seq.length verticesGrid) - 1                
let maxY = maxX                
                
let vertices =
        verticesGrid
        |> Seq.collect id
        |> Set.ofSeq
 
 
let findNeighbours vertex vertices =
    let (x,y) = vertex.Id
    vertices |> Set.filter (fun v ->
                        v.Id = (x+1, y) ||
                        v.Id = (x-1, y) ||
                        v.Id = (x, y+1) ||
                        v.Id = (x, y-1)
                )
    

let rec getShortestDistanceToEnd openSet vertices cnt =
    let current = Set.minElement openSet
    if current.Id = (maxX, maxY)
    then (current.Distance, cnt)
    else
    let neighbours = findNeighbours current vertices
    let shorterNeighbours = neighbours
                            |> Set.filter (fun v -> v.CostOfEntry + current.Distance < v.Distance)
                            |> Set.map (fun v ->
                                {v with Distance = v.CostOfEntry + current.Distance })
                        
                        
    let removeIds = (neighbours |> Set.map (fun v -> v.Id)).Add current.Id
    let verticesLeft = vertices |> Set.filter (fun v -> not (Set.contains v.Id removeIds)) |> Set.union shorterNeighbours
    let newOpenSet = Set.union (openSet |> Set.remove current) shorterNeighbours
    getShortestDistanceToEnd newOpenSet verticesLeft (cnt+1)
 
let startingSet = Set.singleton (Set.minElement vertices)   
let part2 = getShortestDistanceToEnd startingSet vertices 0