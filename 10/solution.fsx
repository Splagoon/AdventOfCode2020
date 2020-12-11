let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map int
    |> List.ofArray
    |> List.sort
    |> fun l -> 0 :: l @ [l |> List.last |> ((+) 3)]

let differences =
    input
    |> List.pairwise
    |> List.map (fun (x, y) -> y - x)

let diffs1, diffs3 =
    differences
    |> Seq.fold (fun (diffs1, diffs3) diff ->
        match diff with
        | 1 -> diffs1+1, diffs3
        | 3 -> diffs1, diffs3+1
        | _ -> diffs1, diffs3)
        (0, 0)

let getFollowupIndexes i =
    let j1 = input.[i]
    let numFollowups =
        input
        |> List.skip (i+1)
        |> List.takeWhile (fun j2 -> j2 - j1 <= 3)
        |> List.length
    seq { for x in 1..numFollowups -> i+x } |> List.ofSeq

let incBy key x map =
    let n =
        match map |> Map.tryFind key with
        | Some n -> n
        | None -> 0uL
    map |> Map.add key (n+x)

let rec findPaths i encounters =
    match getFollowupIndexes i with
    | [] -> encounters
    | followupIndexes ->
        let rec tally nextIndexes encounters =
            match nextIndexes with
            | head :: tail -> tally tail (encounters |> incBy head encounters.[i])
            | _ -> encounters
        findPaths (i+1) (tally followupIndexes encounters)

let paths = findPaths 0 ([0, 1uL] |> Map.ofList)

printfn "Part 1 answer: %i" (diffs1 * diffs3)
printfn "Part 2 answer: %i" paths.[input.Length-1]
