let input = [ 0; 3; 1; 6; 7; 5 ]

let rec doTurn turn lastNum (map: Map<int, int>) =
    seq {
        let num =
            match turn with
            | _ when turn <= input.Length -> input.[turn-1]
            | _ ->
                match map |> Map.tryFind lastNum with
                | None -> 0
                | Some n -> turn - n - 1
        yield num
        yield! doTurn (turn+1) num (map |> Map.add lastNum (turn-1))
    }

let turnSeq = doTurn 1 -1 Map.empty

printfn "Part 1 answer: %i" (turnSeq |> Seq.item 2019)
printfn "Part 2 answer: %i" (turnSeq |> Seq.item 29999999)
