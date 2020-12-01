let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map int

let pairs =
    seq {
        for x in input do
            for y in input do
                yield [x; y]
    }

let trios =
    seq {
        for x in input do
            for y in input do
                for z in input do
                    yield [x; y; z]
    }

let answer =
    Seq.find (List.sum >> ((=) 2020))
    >> List.reduce (*)

printfn "Part 1 answer: %i" (answer pairs)
printfn "Part 2 answer: %i" (answer trios)
