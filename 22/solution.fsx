let parseDeck (s : string) =
    s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.skip 1
    |> Array.map uint64
    |> List.ofArray

let deck1, deck2 =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllText
    |> fun s ->
        match s.Split("\n\n") with
        | [| deck1; deck2 |] ->
            parseDeck deck1, parseDeck deck2
        | _ -> failwithf "Bad input: %s" s

let score deck =
    deck
    |> List.rev
    |> List.mapi (fun i c -> ((uint64 i)+1uL) * c)
    |> List.sum

type PlayResult =
    | P1Win of uint64
    | P2Win of uint64

let rec play1 deck1 deck2 =
    match deck1, deck2 with
    | [], _ -> P2Win (score deck2)
    | _, [] -> P1Win (score deck1)
    | card1 :: tail1, card2 :: tail2 ->
        if card1 > card2
        then play1 (tail1 @ [card1; card2]) tail2
        else play1 tail1 (tail2 @ [card2; card1])

let rec play2 deck1 deck2 memory =
    if memory |> Set.contains (deck1, deck2)
    then P1Win (score deck1)
    else
        let newMemory = memory |> Set.add (deck1, deck2)
        match deck1, deck2 with
        | [], _ -> P2Win (score deck2)
        | _, [] -> P1Win (score deck1)
        | card1 :: tail1, card2 :: tail2 ->
            let winner1 _ = play2 (tail1 @ [card1; card2]) tail2 newMemory
            let winner2 _ = play2 tail1 (tail2 @ [card2; card1]) newMemory
            if tail1.Length >= (int card1) &&
               tail2.Length >= (int card2)
            then
                match play2 (tail1 |> List.take (int card1)) (tail2 |> List.take (int card2)) Set.empty with
                | P1Win _ -> winner1 ()
                | P2Win _ -> winner2 ()
            else
                if card1 > card2
                    then winner1 ()
                    else winner2 ()

let getScore = function
    | P1Win score -> score
    | P2Win score -> score

printfn "Part 1 answer: %i" (play1 deck1 deck2 |> getScore)
printfn "Part 2 answer: %i" (play2 deck1 deck2 Set.empty |> getScore)
