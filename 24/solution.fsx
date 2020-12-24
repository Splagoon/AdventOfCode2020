type Direction =
    | West
    | NorthWest
    | NorthEast
    | East
    | SouthEast
    | SouthWest

// Represents a tile position in a hexagonal grid
type AxialCoord =
    { Q : int
      R : int }
with
    static member (+) (a : AxialCoord, b : AxialCoord): AxialCoord =
        { Q = a.Q + b.Q
          R = a.R + b.R }

    static member (+) (coord : AxialCoord, dir : Direction): AxialCoord =
        // https://www.redblobgames.com/grids/hexagons/#neighbors-axial
        coord +
        match dir with
        | West -> { Q = -1; R = 0 }
        | NorthWest -> { Q = 0; R = -1 }
        | NorthEast -> { Q = 1; R = -1 }
        | East -> { Q = 1; R = 0 }
        | SouthEast -> { Q = 0; R = 1 }
        | SouthWest -> { Q = -1; R = 1 }

let neighbors (coord : AxialCoord) =
    [ West; NorthWest; NorthEast; East; SouthEast; SouthWest ]
    |> List.map (fun d -> coord + d)

type Color =
    | Black
    | White

let tileAt coord map =
    match map |> Map.tryFind coord with
    | Some c -> c
    | None -> White

let flip coord map =
    match map |> tileAt coord with
    | Black -> White
    | White -> Black

let rec parseLine (line : char[]) idx dirs =
    match line |> Array.tryItem idx,
          line |> Array.tryItem (idx+1) with
    | None, None -> dirs |> List.rev
    | Some 's', Some 'e' -> parseLine line (idx+2) (SouthEast :: dirs)
    | Some 's', Some 'w' -> parseLine line (idx+2) (SouthWest :: dirs)
    | Some 'n', Some 'e' -> parseLine line (idx+2) (NorthEast :: dirs)
    | Some 'n', Some 'w' -> parseLine line (idx+2) (NorthWest :: dirs)
    | Some 'w', _ -> parseLine line (idx+1) (West :: dirs)
    | Some 'e', _ -> parseLine line (idx+1) (East :: dirs)
    | _ -> failwithf "Unknown direction in string: %s" (string line)

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map (fun s -> parseLine (s.ToCharArray()) 0 [])

let zeroCoord = { Q = 0; R = 0 }

let walk (dirs : Direction list) =
    dirs
    |> List.fold (+) zeroCoord

let part1 =
    input
    |> Array.fold (fun map dirs ->
        let coord = dirs |> List.fold (+) zeroCoord
        map |> Map.add coord (flip coord map)) Map.empty

let flipDay map =
    map
    |> Map.toSeq
    |> Seq.append
        (map
        |> Map.toSeq
        |> Seq.collect (fst >> neighbors)
        |> Seq.map (fun c -> c, map |> tileAt c))
    |> Set.ofSeq
    |> Set.map (fun (coord, color) ->
        let neighbors = coord |> neighbors |> List.map (fun c -> tileAt c map)
        let blackNeighbors = neighbors |> List.filter ((=) Black) |> List.length
        coord,
        match color, blackNeighbors with
        | Black, n when n = 0 || n > 2 -> White
        | White, n when n = 2 -> Black
        | _ -> color)
    |> Map.ofSeq
    |> fun nextMap -> Some (map, nextMap)

let part2 = Seq.unfold flipDay part1

let blackTiles = Map.toSeq >> Seq.filter (snd >> ((=) Black)) >> Seq.length

printfn "Part 1 answer: %i" (part1 |> blackTiles)
printfn "Part 2 answer: %i" (part2 |> Seq.item 100 |> blackTiles)
