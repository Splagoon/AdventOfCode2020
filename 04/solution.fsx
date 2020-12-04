open System.Text.RegularExpressions

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllText
    |> fun s -> s.Split("\n\n")
    |> Array.map (fun s ->
        s.Split([| '\n'; ' ' |])
        |> Array.choose (fun kvp ->
            kvp.Split(':')
            |> function
            | [| k; v |] -> Some (k, v)
            | [| "" |] -> None
            | a -> failwithf "invalid key-value pair: %A" a)
        |> Map.ofArray)

let neededKeys =
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    |> set

let isValid1 =
    Map.toSeq
    >> Seq.map fst
    >> Set.ofSeq
    >> Set.isSubset neededKeys

let regexes =
    [ "byr", "^(19[2-9][0-9]|200[0-2])$"
      "iyr", "^(201[0-9]|2020)$"
      "eyr", "^(202[0-9]|2030)$"
      "hgt", "^((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)$"
      "hcl", "^(#[0-9a-f]{6})$"
      "ecl", "^(amb|blu|brn|gry|grn|hzl|oth)$"
      "pid", "^(\d{9})$" ]
    |> List.map (fun (k, v) -> k, Regex(v))

let isValid2 passport =
    regexes
    |> List.map (fun (k, r) ->
        match Map.tryFind k passport with
        | None -> false
        | Some v -> r.IsMatch(v))
    |> List.reduce (&&)

printfn "Part 1 answer: %i" (input |> Seq.filter isValid1 |> Seq.length)
printfn "Part 2 answer: %i" (input |> Seq.filter isValid2 |> Seq.length)
