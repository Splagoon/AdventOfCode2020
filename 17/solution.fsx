
let neighbors (coord : int list) : int list list =
    let rec permute n found =
        match n with
        | 0 -> found
        | _ ->
            let newFound = 
                found
                |> List.collect (fun (l : int list) ->
                    [ -1 :: l ;
                       0 :: l ;
                       1 :: l ])
            permute (n-1) newFound

    permute coord.Length [[]]
    |> List.map (List.mapi (fun i n -> coord.[i]+n))
    |> List.filter ((<>) coord)

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.mapi (fun y ->
        Seq.mapi (fun x c ->
            match c with
            | '#' -> Some [ x; y ]
            | _ -> None))
    |> Seq.concat
    |> Seq.choose id
    |> Set.ofSeq

let simulate allActive =
    let allInactive =
        allActive
        |> Seq.collect neighbors
        |> Set.ofSeq
        |> (fun s -> Set.difference s allActive)

    let activeNeighbors coord =
        coord
        |> neighbors
        |> Set.ofSeq
        |> Set.intersect allActive
        |> Set.count

    let rec simulateActive active state =
        match active with
        | head :: tail ->
            let newState =
                if head |> activeNeighbors |> fun l -> l = 2 || l = 3
                then state // active -> active
                else state |> Set.remove head // active -> inactive
            simulateActive tail newState
        | _ -> state

    let rec simulateInactive inactive state =
        match inactive with
        | head :: tail ->
            let newState =
                if head |> activeNeighbors |> (=) 3
                then state |> Set.add head // inactive -> active
                else state // inactive -> inactive
            simulateInactive tail newState
        | _ -> state

    allActive
    |> simulateActive (allActive |> Set.toList)
    |> simulateInactive (allInactive |> Set.toList)

let simulation n =
    let rec toSeq state =
        seq {
            yield state
            yield! toSeq (simulate state)
        }
    let inputN =
        input
        |> Set.map (fun xy ->
            xy @ (List.replicate (n-2) 0))
    toSeq inputN

printfn "Part 1 answer: %i" (simulation 3 |> Seq.item 6 |> Set.count)
printfn "Part 2 answer: %i" (simulation 4 |> Seq.item 6 |> Set.count)
