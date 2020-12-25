let cardKey, doorKey =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map uint64
    |> function
    | [| a; b |] -> a, b
    | _ -> failwith "Bad input"

let transform subject =
    let loop value =
        let nextValue = (value * subject) % 20201227uL
        Some (value, nextValue)
    Seq.unfold loop 1uL

let cardLoop = (transform 7uL |> Seq.findIndex ((=) cardKey))

printfn "Part 1 answer: %i" (transform doorKey |> Seq.item cardLoop)
