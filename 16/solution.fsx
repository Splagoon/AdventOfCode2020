open System.Text.RegularExpressions

type Field =
    { Name : string
      Range1 : uint64 * uint64
      Range2 : uint64 * uint64 }

let fieldRegex = Regex("([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)", RegexOptions.Compiled)
let parseField =
    fieldRegex.Match
    >> fun m ->
        let group (g : int) = uint64 m.Groups.[g].Value
        { Name = m.Groups.[1].Value
          Range1 = group 2, group 3
          Range2 = group 4, group 5 }

let parseTicket (ticket : string) =
    ticket.Split(",") |> Array.map uint64

let valid field value =
    match field.Range1, field.Range2 with
    | (min, max), _ when value >= min && value <= max -> true
    | _, (min, max) when value >= min && value <= max -> true
    | _ -> false

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines

let fieldsInput, myTicketInput, otherTicketsInput =
    input
    |> Array.splitAt (input |> Array.findIndex ((=) "your ticket:"))
    |> fun (top, latter) ->
        top |> Array.take (top.Length-1),
        latter.[1],
        latter |> Array.skip 4

let fields = fieldsInput |> Seq.map parseField

let myTicket =
    myTicketInput |> parseTicket

let otherTickets =
    otherTicketsInput
    |> Seq.map parseTicket

let anyValid value =
    fields |> Seq.exists (fun field -> valid field value)

let goodTickets =
    otherTickets
    |> Seq.filter (Array.forall anyValid)

let columns =
    let maxCol =
        goodTickets
        |> Seq.head
        |> Array.length
        |> (fun l -> l-1)

    [| 0..maxCol |]
    |> Array.map (fun i ->
        goodTickets
        |> Seq.map (fun fields -> fields.[i]))

let fieldMappings =
    columns
    |> Array.map (fun vals ->
        fields
        |> Seq.filter (fun field ->
            vals |> Seq.forall (valid field))
        |> Set.ofSeq)

let rec reduceFieldMappings (fieldMapping : Set<Field> array) solvedFields =
    if fieldMapping |> Array.forall (Seq.length >> ((=) 1))
    then fieldMapping |> Array.map Seq.exactlyOne
    else
        let fieldToSolve =
            fieldMapping
            |> Array.find (fun a ->
                match a |> Seq.toList with
                | [ one ] -> solvedFields |> Set.contains one |> not
                | _ -> false)
            |> Seq.exactlyOne

        reduceFieldMappings
            (fieldMapping
            |> Array.map (fun set ->
                if set.Count = 1
                then set
                else set |> Set.remove fieldToSolve))
            (solvedFields |> Set.add fieldToSolve)

let fieldMapping = reduceFieldMappings fieldMappings Set.empty

printfn "Part 1 answer: %i"
    (otherTickets
    |> Seq.concat
    |> Seq.filter (anyValid >> not)
    |> Seq.sum)

printfn "Part 2 answer: %i"
    (fieldMapping
    |> Seq.mapi (fun i f -> i, f.Name)
    |> Seq.choose (fun (i, n) ->
        if n.StartsWith("departure")
        then Some myTicket.[i]
        else None)
    |> Seq.reduce (*))
