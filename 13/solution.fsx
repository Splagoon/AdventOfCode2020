type Bus =
    | BusId of uint64
    | Unknown

let timeCanDepart, busses =
    let input =
        sprintf "%s/input.txt" __SOURCE_DIRECTORY__
        |> System.IO.File.ReadAllLines
    match input with
    | [| timeCanDepart; busses |] ->
        uint64 timeCanDepart, busses.Split(",") 
                              |> Array.map (function
                              | "x" -> Unknown
                              | x -> BusId (uint64 x))
    | _ -> failwith "Invalid input"

let earliestBus, earliestDeparture =
    busses
    |> Seq.choose (function
        | BusId id -> Some (id, seq { 0uL..id..System.UInt64.MaxValue })
        | Unknown -> None)
    |> Seq.map (fun (id, schedule) -> id, schedule |> Seq.find ((<=) timeCanDepart))
    |> Seq.minBy snd

let contestTimestamp =
    let input =
        busses
        |> Seq.mapi (fun i -> function
            | BusId id -> Some (uint64 i, id)
            | Unknown -> None)
        |> Seq.choose id
        |> List.ofSeq

    let rec loop busses timestamp multiplier =
        match busses with
        | ((_, cur), (offset, next)) :: tail ->
            let newMult = multiplier * cur
            let rec getNextTimestamp timestamp =
                if (timestamp + offset) % next = 0uL
                then timestamp
                else getNextTimestamp (timestamp + newMult)
            loop tail (getNextTimestamp timestamp) newMult
        | _ -> timestamp
    loop (input |> List.pairwise) 0uL 1uL

printfn "Part 1 answer: %i" (earliestBus * (earliestDeparture - timeCanDepart))
printfn "Part 2 answer: %i" contestTimestamp
