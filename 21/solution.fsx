open System.Text.RegularExpressions

type Food =
    { Ingredients : Set<string>
      Allergens: string list }

let regex =
    Regex("([\w ]+) \(contains ([\w ,]+)\)", RegexOptions.Compiled)

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map (fun s ->
        match regex.Match(s) with
        | m when m.Success ->
            { Ingredients = m.Groups.[1].Value.Split(" ") |> Set.ofArray
              Allergens = m.Groups.[2].Value.Split(", ") |> List.ofArray }
        | _ -> failwithf "Invalid food: %s" s)
    |> List.ofArray

let rec getAllergenMap foods map =
    let rec loopAllergens allergens ingredients map =
        match allergens with
        | allergen :: tail ->
            let possibleIngredients =
                match map |> Map.tryFind allergen with
                | Some i -> Set.intersect i ingredients
                | None -> ingredients
            loopAllergens tail ingredients (map |> Map.add allergen possibleIngredients)
        | [] -> map

    match foods with
    | food :: tail ->
        getAllergenMap tail (loopAllergens food.Allergens food.Ingredients map)
    | [] -> map

let allergenMap = getAllergenMap input Map.empty

let unsafeIngredients =
    allergenMap
    |> Map.toSeq
    |> Seq.map snd
    |> Set.unionMany

let rec reduceAllergens map solved =
    match
        map
        |> Map.toSeq
        |> Seq.tryFind (fun (a, b) ->
            match b |> Seq.tryExactlyOne with
            | Some _ -> solved |> Set.contains a |> not
            | _ -> false)
        |> Option.map (fun (a, b) -> a, b |> Seq.exactlyOne) with
    | Some (allergen, ingredient) ->
        reduceAllergens
            (map
            |> Map.toSeq
            |> Seq.map (fun (a, b) ->
                a,
                if a = allergen then b
                else b |> Set.remove ingredient)
            |> Map.ofSeq)
            (solved |> Set.add allergen)
    | None ->
        map
        |> Map.toSeq
        |> Seq.choose (fun (a, b) ->
            match b |> Seq.tryExactlyOne with
            | Some one -> Some (a, one)
            | None -> None)

let finalAllergenMap = reduceAllergens allergenMap Set.empty

printfn "Part 1 answer: %i"
    (input
    |> Seq.collect (fun f ->f.Ingredients)
    |> Seq.filter (unsafeIngredients.Contains >> not)
    |> Seq.length)

printfn "Part 2 answer: %s"
    (finalAllergenMap
    |> Seq.sortBy fst
    |> Seq.map snd
    |> String.concat ",")
