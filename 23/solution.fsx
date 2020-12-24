open System.Collections.Generic

let input1 =
    "598162734"
    |> Seq.map (string >> uint64)
    |> LinkedList

let mutable input2 =
    seq {
        yield! input1
        for i in (input1.Count+1)..1000000 do
            yield (uint64 i)
    } |> LinkedList

let rec findTarget n numCups cupsToSkip =
    if n = 0uL
    then findTarget numCups numCups cupsToSkip
    else
        if cupsToSkip |> Set.contains n
        then findTarget (n-1uL) numCups cupsToSkip
        else n

let rec move (cupLookup : Map<uint64, LinkedListNode<uint64>>) (cups : LinkedList<uint64>) =
    let current = cups.First
    let pickUp =
        [ cups.First.Next
          cups.First.Next.Next
          cups.First.Next.Next.Next ]

    let target =
        findTarget
            (current.Value-1uL)
            (uint64 cups.Count)
            (pickUp |> Seq.map (fun it -> it.Value) |> Set.ofSeq)

    for _ in 1..4 do
        cups.RemoveFirst()

    let targetNode = cupLookup.[target]

    for i in pickUp |> Seq.rev do
        cups.AddAfter(targetNode, i) |> ignore

    cups.AddLast(current) |> ignore

    Some (cups, cups)

let moves (input : LinkedList<uint64>) =
    let rec buildLookup (node : LinkedListNode<uint64>) lookup =
        if node |> isNull
        then lookup
        else
            buildLookup node.Next (lookup |> Map.add node.Value node)

    let lookup = buildLookup input.First Map.empty
    Seq.unfold (move lookup) input

let cupsAfter1 (cups : LinkedList<uint64>) =
    let rec loop _ =
        if cups.First.Value = 1uL
        then cups
        else
            cups.AddLast(cups.First.Value) |> ignore
            cups.RemoveFirst()
            loop ()
    loop () |> Seq.tail

printfn "Part 1 answer: %s"
    (moves input1
    |> Seq.item 99
    |> cupsAfter1
    |> Seq.map string
    |> String.concat "")

printfn "Part 2 answer: %i"
    (moves input2
    |> Seq.item 10000000
    |> cupsAfter1
    |> Seq.take 2
    |> Seq.reduce (*))
