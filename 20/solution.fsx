type Tile =
    { Id : uint64
      Data : bool[,] }
with
    member this.TopEdge = this.Data.[*, 0]
    member this.RightEdge = this.Data.[9, *]
    member this.BottomEdge = this.Data.[*, 9]
    member this.LeftEdge = this.Data.[0, *]

let flipHorizontal (data : bool[,]) =
    seq {
        for col in 0..((data |> Array2D.length1) - 1) ->
            data.[col, *] |> Array.rev
    } |> array2D

let rotate90 (data : bool[,]) =
    seq {
        for row in 0..((data |> Array2D.length2) - 1) ->
            data.[*, row] |> Array.rev
    } |> array2D

let allOrientations data =
    [ id
      flipHorizontal
      rotate90
      rotate90 >> flipHorizontal
      rotate90 >> rotate90
      rotate90 >> rotate90 >> flipHorizontal
      rotate90 >> rotate90 >> rotate90
      rotate90 >> rotate90 >> rotate90 >> flipHorizontal ]
    |> List.map (fun f -> f data)

let edgeMatch edge1 edge2 =
    Array.map2 (=) edge1 edge2
    |> Array.reduce (&&)

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllText
    |> fun s -> s.Split("\n\n")
    |> Seq.map (fun s ->
        let lines = s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
        let id = lines.[0].Substring(5, 4) |> uint64
        lines
        |> Array.skip 1
        |> Array.map (Seq.map (function
            | '#' -> true
            | '.' -> false
            | c -> failwithf "Bad character in data: %c" c))
        |> array2D
        |> fun data -> { Id = id; Data = data })
        // |> allOrientations
        // |> List.map (fun data -> { Id = id; Data = data }))

let maxIndex =
    input
    |> Seq.length
    |> double
    |> sqrt
    |> int
    |> fun n -> n-1

type SearchState =
    { UnusedTiles : Tile seq
      FoundTiles : (Tile option)[,]
      NextPosition : int * int }

let zeroState =
    { UnusedTiles =
        input
        |> Seq.collect (fun t ->
            t.Data
            |> allOrientations
            |> Seq.map (fun d -> { t with Data = d }))
      FoundTiles = Array2D.zeroCreate (maxIndex+1) (maxIndex+1)
      NextPosition = 0, 0 }

let copyAndUpdate2D x y value =
    Array2D.mapi (fun u v item ->
        if x = u && y = v then value
        else item)

let toSeq array =
    seq {
        for x in 0..((array |> Array2D.length1) - 1) do
            for y in 0..((array |> Array2D.length2) - 1) ->
                array.[x, y]
    }

let rec search (searchStates : SearchState list) =
    match searchStates with
    | searchState :: tail ->
        if searchState.FoundTiles |> toSeq |> Seq.forall Option.isSome
        then
            searchState.FoundTiles
            |> Array2D.map (function
                | Some x -> x
                | None -> failwith "???")
        else
            let x, y = searchState.NextPosition
            let nextPos =
                if x = maxIndex
                then 0, y+1
                else x+1, y

            let newSearchStates =
                searchState.UnusedTiles
                |> Seq.filter (fun tile ->
                    // Match left tile
                    if x = 0
                    then true
                    else
                        match searchState.FoundTiles.[x-1, y] with
                        | Some leftTile -> edgeMatch tile.LeftEdge leftTile.RightEdge
                        | None -> failwith "Left tile is missing")
                |> Seq.filter (fun tile ->
                    // Match above tile
                    if y = 0
                    then true
                    else
                        match searchState.FoundTiles.[x, y-1] with
                        | Some aboveTile -> edgeMatch tile.TopEdge aboveTile.BottomEdge
                        | None -> failwith "Above tile is missing")
                |> Seq.map (fun tile ->
                    { NextPosition = nextPos
                      UnusedTiles = searchState.UnusedTiles |> Seq.filter (fun t -> t.Id <> tile.Id)
                      FoundTiles = searchState.FoundTiles |> copyAndUpdate2D x y (Some tile) })
                |> Seq.toList

            search (tail @ newSearchStates)

    | [] -> failwith "Exhausted search options"

let solution = search [zeroState]

let removeBorder (data : 'T[,]) =
    let size = data |> Array2D.length1
    data.[1..(size-2), 1..(size-2)]

let combine (data : 'T[,][,]) : 'T[,] =
    let outerSize = data |> Array2D.length1
    let innerSize = data.[0,0] |> Array2D.length1
    let getRow r =
        let outerRow = r / innerSize
        let innerRow = r % innerSize
        seq {
            for block in data.[*, outerRow] ->
                block.[*, innerRow]
        } |> Seq.concat |> Seq.toArray

    let lastRow = (outerSize * innerSize) - 1
    seq {
        for r in 0..lastRow ->
            getRow r
    } |> array2D

let finalImages =
    solution
    |> Array2D.map (fun t -> removeBorder t.Data)
    |> combine
    |> allOrientations

let seaMonsterVolume = 15
let seaMonsterWidth = 20
let seaMonsterHeight = 3
let seaMonsterMask =
    @"
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
"
    |> fun s -> s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.mapi (fun y ->
        Seq.mapi (fun x -> function
        | '#' -> Some (x, y)
        | _ -> None)
        >> Seq.choose id)
    |> Seq.concat

let seaMonsterAt data (x, y) =
    if x + seaMonsterWidth >= (data |> Array2D.length1) ||
       y + seaMonsterHeight >= (data |> Array2D.length2)
    then false
    else
        seaMonsterMask
        |> Seq.forall (fun (maskX, maskY) ->
            data.[x + maskX, y + maskY])

let countSeaMonsters data =
    data
    |> Array2D.mapi (fun x y _ -> x, y)
    |> toSeq
    |> Seq.filter (seaMonsterAt data)
    |> Seq.length

let numSeaMonsters =
    finalImages
    |> Seq.map countSeaMonsters
    |> Seq.find ((<>) 0)

printfn "Part 1 answer: %i"
    ([ solution.[0, 0]
       solution.[0, maxIndex]
       solution.[maxIndex, 0]
       solution.[maxIndex, maxIndex] ]
    |> List.map (fun t -> t.Id)
    |> List.reduce (*))

printfn "Part 2 answer: %i"
    (finalImages.[0]
    |> toSeq
    |> Seq.filter id
    |> Seq.length
    |> fun n -> n - (numSeaMonsters * seaMonsterVolume))
