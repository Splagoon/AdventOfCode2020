open System

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> IO.File.ReadAllText
    |> fun s -> s.Split("\n\n")
    |> Seq.map (fun s ->
        s.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Set.ofSeq)

let count fn =
    input |> Seq.sumBy (fn >> Set.count)

printfn "Part 1 answer: %i" (count Set.unionMany)
printfn "Part 2 answer: %i" (count Set.intersectMany)
