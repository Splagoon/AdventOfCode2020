type Position =
    | Floor
    | Wall
    | EmptySeat
    | OccupiedSeat

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map (Seq.map (function
        | '.' -> Floor
        | 'L' -> EmptySeat
        | x -> failwithf "Not a valid character: %c" x)
        >> Array.ofSeq)

let width = input.[0].Length
let height = input.Length

let getPos x y (map : Position array array) =
    match x, y with
    | x, y when x < 0 || x >= width || y < 0 || y >= height -> Wall
    | x, y -> map.[y].[x]

let deltas =
    [ -1, -1; 0, -1; 1, -1 
      -1,  0;        1,  0
      -1,  1; 0,  1; 1,  1 ]

let getAdjacent x y map =
    deltas
    |> List.map (fun (dx, dy) -> getPos (x+dx) (y+dy) map)

let rule1 x y map =
    let adjacentOccupied =
        getAdjacent x y map
        |> Seq.filter ((=) OccupiedSeat)
        |> Seq.length
    match getPos x y map with
    | Floor -> Floor, false
    | EmptySeat when adjacentOccupied = 0 -> OccupiedSeat, true
    | OccupiedSeat when adjacentOccupied >= 4 -> EmptySeat, true
    | s -> s, false

let rule2 x y map =
    let rec walk (x, y) (dx, dy) =
        let newX, newY = x+dx, y+dy
        match getPos newX newY map with
        | OccupiedSeat -> true
        | Wall | EmptySeat -> false
        | Floor -> walk (newX, newY) (dx, dy)

    let visibleOccupied () =
        deltas
        |> List.map (walk (x, y))
        |> Seq.filter id
        |> Seq.length

    match getPos x y map with
    | EmptySeat when (visibleOccupied ()) = 0 -> OccupiedSeat, true
    | OccupiedSeat when (visibleOccupied ()) >= 5 -> EmptySeat, true
    | s -> s, false

let rec simulate rule map =
    let res =
        map
        |> Array.mapi (fun y ->
            Array.mapi (fun x _ ->
                rule x y map))
    let newMap = res |> Array.map (Array.map fst)
    let changed = res |> Array.map ((Array.map snd) >> (Array.reduce (||))) |> Array.reduce (||)
    if changed
    then simulate rule newMap
    else newMap

let countOccupied rule =
    simulate rule input
    |> Seq.collect id
    |> Seq.filter ((=) OccupiedSeat)
    |> Seq.length

printfn "Part 1 answer: %i" (countOccupied rule1)
printfn "Part 2 answer: %i" (countOccupied rule2)
