open System.Text.RegularExpressions

let bagRegex = Regex(@"^(\w+ \w+) bags contain", RegexOptions.Compiled)
let bagQuantityRegex = Regex(@"(\d+) (\w+ \w+) bags?(,|.$)", RegexOptions.Compiled)

let parse line =
    let bag, searchStart =
        bagRegex.Match(line, 0)
        |> fun m -> m.Groups.[1].Value, m.Index + m.Length

    let rec getContents (m : Match) contents =
        if m.Success
        then
            let num = int m.Groups.[1].Value
            let bag = m.Groups.[2].Value
            getContents (m.NextMatch()) ((bag, num) :: contents)
        else contents

    (bag, getContents (bagQuantityRegex.Match(line, searchStart)) [])

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Seq.map parse
    |> Seq.toList

let rec addInverseEntry key values map =
    match values with
    | (bag, _) :: tail ->
        match Map.tryFind bag map with
        | None -> addInverseEntry key tail (Map.add bag [key] map)
        | Some l -> addInverseEntry key tail (Map.add bag (key :: l) map)
    | _ -> map

let rec getInverseMap entries map =
    match entries with
    | [] -> map
    | (bag, entries) :: tail ->
        getInverseMap tail (addInverseEntry bag entries map)

let inverseMap = getInverseMap input Map.empty
let normalMap = Map.ofSeq input

let rec getAllContainers colorsToCheck checkedColors =
    match colorsToCheck with
    | color :: tail ->
        if checkedColors |> Set.contains color
        then getAllContainers tail checkedColors
        else
            let newSet = checkedColors |> Set.add color
            match inverseMap |> Map.tryFind color with
            | Some moreColors ->
                getAllContainers (tail @ moreColors) newSet
            | None ->
                // This happens when `color` cannot contain other bags
                getAllContainers tail newSet
    | _ -> checkedColors

let colorsThatCanContainShinyGold =
    getAllContainers ["shiny gold"] Set.empty
    |> Set.remove "shiny gold"

let rec getNumBags bagsToCheck total =
    match bagsToCheck with
    | (bag, m) :: tail ->
        match normalMap.[bag] with
        | [] -> getNumBags tail (total + m)
        | bags ->
            let moreBags = bags |> List.map (fun (b, n) -> (b, n*m))
            getNumBags (tail @ moreBags) (total + m)
    | _ -> total

printfn "Part 1 answer: %i" (colorsThatCanContainShinyGold |> Set.count)
printfn "Part 2 answer: %i" ((getNumBags ["shiny gold", 1] 0) - 1) // exclude the outermost bag
