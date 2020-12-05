type Seat =
    { Row : int
      Col : int }
with
    member this.Id = (this.Row * 8) + this.Col

let parse (bspStr : string) =
    let rec findRow bsp rows =
        match rows |> List.splitInto 2, bsp with
        | [ front; _ ], 'F' :: tail -> findRow tail front
        | [ _; back ], 'B' :: tail -> findRow tail back
        | _ -> rows

    let rec findCol bsp cols =
        match cols |> List.splitInto 2, bsp with
        | [ left; _ ], 'L' :: tail -> findCol tail left
        | [ _; right ], 'R' :: tail -> findCol tail right
        | _ -> cols

    let rowBsp = bspStr.Substring(0, 7) |> Seq.toList
    let colBsp = bspStr.Substring(7) |> Seq.toList

    { Row = findRow rowBsp [ 0..127 ] |> List.exactlyOne
      Col = findCol colBsp [ 0..7 ] |> List.exactlyOne }

let findMissing ids =
    let allSeats =
        [ ids |> Set.minElement .. ids |> Set.maxElement ]
        |> Set.ofList
    Set.difference allSeats ids
    |> Set.maxElement

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Seq.map (parse >> (fun it -> it.Id))
    |> Set.ofSeq

printfn "Part 1 answer: %i" (input |> Set.maxElement)
printfn "Part 2 answer: %i" (input |> findMissing)
