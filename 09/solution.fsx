let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map uint64

let inputWithPreambles =
    seq {
        for i in 25..(input.Length-1) ->
            input.[i], Array.sub input (i-25) 25
    }

let combinations array =
    seq {
        for x in array do
            for y in array do
                if x <> y then yield x, y
    }

let valid (num, preamble) =
    match preamble
          |> combinations
          |> Seq.tryFind (fun (x, y) -> x + y = num) with
    | Some _ -> true
    | None -> false

let invalidNum = inputWithPreambles |> Seq.find (valid >> not) |> fst

let rec findWeakRange i n =
    let range = Array.sub input i n
    match range |> Array.sum with
    | x when x < invalidNum -> findWeakRange i (n+1)
    | x when x > invalidNum -> findWeakRange (i+1) 2
    | _ -> range

let weakRange = findWeakRange 0 2

printfn "Part 1 answer: %i" invalidNum
printfn "Part 2 answer: %i" ((weakRange |> Array.min) + (weakRange |> Array.max))
