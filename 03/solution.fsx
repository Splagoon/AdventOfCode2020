let width = 31

type Obstacle =
    | None
    | Tree

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map
        (Seq.map (function
            | '#' -> Tree
            | _ -> None)
         >> Array.ofSeq)

let isTree (row : Obstacle array) column =
    match row.[column % width] with
    | Tree -> true
    | _ -> false

let numTrees (dx, dy) =
    seq {
        for row in 0 .. dy .. (input.Length - 1) ->
            isTree input.[row] ((row / dy) * dx)
    }
    |> Seq.filter id
    |> Seq.length

printfn "Part 1 answer: %i" (numTrees (3, 1))

printfn
    "Part 2 answer: %i"
    ([ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
     |> Seq.map numTrees
     |> Seq.reduce (*))
