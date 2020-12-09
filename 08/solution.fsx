type Result =
    | Executing
    | InfiniteLoop
    | Terminated

type State =
    { Line : int
      Accumulator : int
      Instructions : (string * int) list
      VisitedLines : Set<int>
      Result : Result }

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Seq.map (fun line ->
        match line.Split(" ") with
        | [| instruction; arg |] -> instruction, int arg
        | _ -> failwithf "Bad line: %s" line)
    |> List.ofSeq

let rec execute state =
    if state.Line = state.Instructions.Length
    then { state with Result = Terminated }
    else
        let newVisited = state.VisitedLines |> Set.add state.Line
        match (state.Instructions).[state.Line] with
        | "acc", arg ->
            execute { state with Accumulator = state.Accumulator + arg
                                 Line = state.Line + 1
                                 VisitedLines = newVisited }
        | "jmp", arg ->
            let newLine = state.Line + arg
            if state.VisitedLines |> Set.contains newLine
            then { state with Result = InfiniteLoop }
            else
                execute { state with Line = state.Line + arg
                                     VisitedLines = newVisited }
        | "nop", _ ->
            execute { state with Line = state.Line + 1 }
        | i, _ -> failwithf "Unknown instruction: %s" i

let startState instructions =
    { Line = 0
      Accumulator = 0
      Instructions = instructions
      VisitedLines = Set.empty
      Result = Executing }

let endState1 = startState input |> execute

let patchLines =
    input
    |> List.mapi (fun line instruction ->
        match instruction with
        | "jmp", _ -> Some line
        | "nop", _ -> Some line
        | _ -> None)
    |> List.choose id

let patchedInputs =
    seq {
        for line in patchLines ->
            input
            |> List.mapi (fun l instruction ->
                match l = line, instruction with
                | true, ("jmp", arg) -> "nop", arg
                | true, ("nop", arg) -> "jmp", arg
                | _ -> instruction)
    }

let endState2 =
    patchedInputs
    |> Seq.map (startState >> execute)
    |> Seq.find (fun state -> state.Result = Terminated)

printfn "Part 1 answer: %i" endState1.Accumulator
printfn "Part 2 answer: %i" endState2.Accumulator
