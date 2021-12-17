module AdventCalendar2021.Day15Part1

[<CustomComparison; CustomEquality>]
type Vertex =
    {CostOfEntry: int; Distance: int; Id:int*int}
    
    interface System.IComparable with
        member x.CompareTo yObj =
            match yObj with
            | :? Vertex as y ->
                if (fst x.Id = fst y.Id) && (snd x.Id = snd y.Id)
                then 0
                else
                if (x.Distance = y.Distance) then -1 else x.Distance.CompareTo y.Distance
            | _ -> invalidArg "yObj" "can only compare Vertex with other Vertex"
    override x.GetHashCode() = hash x            
    override x.Equals(yObj) =
        match yObj with
        | :? Vertex as y -> x.Id = y.Id
        | _ -> false

let lineToVertices lineIndex (line:string) =
    line
    |> Seq.mapi (fun colIndex char ->
        let cost = if lineIndex = 0 && colIndex = 0 then 0 else System.Globalization.CharUnicodeInfo.GetDigitValue char
        {Id = (colIndex, lineIndex); CostOfEntry = cost; Distance = if lineIndex = 0 && colIndex = 0 then 0 else System.Int32.MaxValue})

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
    

let rec getShortestDistanceToEnd vertices cnt =
    let current = Set.minElement vertices
    if current.Id = (maxX, maxY)
    then (current.Distance, cnt)
    else
    let neighbours = findNeighbours current vertices
    let newNeighbours = neighbours
                        |> Set.map (fun v ->
                            let distance = v.CostOfEntry + current.Distance
                            {v with Distance = if distance < v.Distance then distance else v.Distance })
                        
    let removeIds = (neighbours |> Set.map (fun v -> v.Id)).Add current.Id
    let v1 = vertices |> Set.filter (fun v -> not (Set.contains v.Id removeIds))
    let newVertices = Set.union v1 newNeighbours
    getShortestDistanceToEnd newVertices (cnt+1)
     
let part1 = getShortestDistanceToEnd vertices 0