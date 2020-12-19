type Operator =
    | Noop
    | Add
    | Multiply

type State =
    { Value : uint64 option
      Tokens : char list
      Advanced : bool }

let zeroState advanced tokens =
    { Value = None
      Tokens = tokens
      Advanced = advanced }

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map (fun s ->
        s.Replace(" ", "") |> Seq.toList)

let rec eval state =
    let rec grabNumber = function
        | '(' :: tail ->
            let v, tail = eval (zeroState state.Advanced tail)
            trySeek v tail
        | d :: tail when System.Char.IsDigit(d) ->
            let v = uint64 d - uint64 '0'
            trySeek v tail
        | _ -> failwith "Unexpected token where number should be"
    and evalOp op lhs tail : State =
        let opFn =
            match op with
            | Noop -> failwith "Attempted to apply noop"
            | Add -> (+)
            | Multiply -> (*)
        let rhs, newTail = grabNumber tail
        { state with Value = Some (opFn lhs rhs); Tokens = newTail }
    and trySeek v tail =
        match state.Advanced, tail with
        | true, '+' :: nextTail ->
            match evalOp Add v nextTail with
            | { Value = Some v; Tokens = t } -> v, t
            | _ -> failwith "Expected value after operator, got none"
        | _, tail -> v, tail

    match state.Value, state.Tokens with
    | None, tokens ->
        let startingValue, tail = grabNumber tokens
        eval { state with Value = Some startingValue; Tokens = tail }
    | Some x, '+' :: tail -> eval (evalOp Add x tail)
    | Some x, '*' :: tail -> eval (evalOp Multiply x tail)
    | Some x, ')' :: tail -> x, tail
    | Some x, [] -> x, []
    | x -> failwithf "Bad state: %A" x

printfn "Part 1 answer: %i" (input |> Array.sumBy (zeroState false >> eval >> fst))
printfn "Part 2 answer: %i" (input |> Array.sumBy (zeroState true >> eval >> fst))
