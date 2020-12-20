open System.Text.RegularExpressions

let regex s =
    Regex(s, RegexOptions.Compiled)

type Rule =
    | ExactRule of char
    | MultiRule of uint64 list
    | BooleanRule of uint64 list * uint64 list

let exactRuleRegex =
    "^(\d+): \"(\w)\"$" |> regex
let multiRuleRegex =
    "^(\d+):((?: \d+)+)$" |> regex
let booleanRuleRegex =
    "^(\d+):((?: \d+)+) \|((?: \d+)+)$" |> regex

let parseNumbers (str : string) =
    str.Split(' ')
    |> Array.toList
    |> List.tail
    |> List.map uint64

let parseRule str =
    match exactRuleRegex.Match(str),
          multiRuleRegex.Match(str),
          booleanRuleRegex.Match(str) with
    | m, _, _ when m.Success ->
        uint64 m.Groups.[1].Value, ExactRule m.Groups.[2].Value.[0]
    | _, m, _ when m.Success ->
        uint64 m.Groups.[1].Value, MultiRule (m.Groups.[2].Value |> parseNumbers)
    | _, _, m when m.Success ->
        uint64 m.Groups.[1].Value,
        BooleanRule (m.Groups.[2].Value |> parseNumbers, m.Groups.[3].Value |> parseNumbers)
    | _ -> failwithf "Invalid rule: %s" str

let ruleMap, inputs =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> fun arr ->
        let idx = arr |> Array.findIndex ((=) "")
        let front, back = arr |> Array.splitAt idx
        front |> Array.map parseRule |> Map.ofArray,
        back |> Array.skip 1 |> Array.map Seq.toList

let patchedRuleMap =
    ruleMap
    |> Map.add 8uL (BooleanRule ([ 42uL ], [ 42uL; 8uL ]))
    |> Map.add 11uL (BooleanRule ([ 42uL; 31uL ], [ 42uL; 11uL; 31uL ]))

let rec eval (ruleMap : Map<uint64, Rule>) rule input : char list list option=
    let rec evalMulti rules (input : char list) : char list list option =
        match rules with
        | rule :: ruleTail ->
            match eval ruleMap ruleMap.[rule] input with
            | Some inputTails ->
                let results = 
                    inputTails
                    |> List.map (evalMulti ruleTail)
                if results |> List.forall Option.isNone
                then None
                else Some (results |> List.choose id |> List.concat)
            | None -> None
        | [] -> Some [ input ]

    match rule, input with
    | ExactRule a, b :: tail when a = b -> Some [ tail ]
    | MultiRule rules, _ -> evalMulti rules input
    | BooleanRule (rules1, rules2), _ ->
        match evalMulti rules1 input,
              evalMulti rules2 input with
        | Some tail1, Some tail2 -> Some (tail1 @ tail2)
        | Some tail1, None -> Some tail1
        | None, Some tail2 -> Some tail2
        | None, None -> None
    | _ -> None

let matchesRule0 (ruleMap : Map<uint64, Rule>) =
    inputs
    |> Array.filter (fun input ->
        match eval ruleMap ruleMap.[0uL] input with
        | Some l when l |> List.contains List.empty -> true
        | _ -> false)
    |> Array.length

printfn "Part 1 answer: %i" (matchesRule0 ruleMap)
printfn "Part 2 answer: %i" (matchesRule0 patchedRuleMap)
