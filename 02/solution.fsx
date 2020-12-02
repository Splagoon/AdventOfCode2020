open System.Text.RegularExpressions

type Record =
    { Min : int
      Max : int
      Char : char
      Password : string }

let parse line =
    let m = Regex.Match(line, @"(\d+)-(\d+) (.): (.+)")
    { Min = int m.Groups.[1].Value
      Max = int m.Groups.[2].Value
      Char = char m.Groups.[3].Value
      Password = m.Groups.[4].Value }

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map parse

let valid1 record =
    let charCount =
        record.Password
        |> Seq.filter ((=) record.Char)
        |> Seq.length

    charCount >= record.Min && charCount <= record.Max

let valid2 record =
    let indexes =
        record.Password
        |> Seq.mapi (fun i c -> (i+1), c)
        |> Seq.filter (snd >> ((=) record.Char))
        |> Seq.map fst
    match Seq.contains record.Min indexes, Seq.contains record.Max indexes with
    | true, false -> true
    | false, true -> true
    | _ -> false

printfn "Part 1 answer: %i" (input |> Seq.filter valid1 |> Seq.length)
printfn "Part 2 answer: %i" (input |> Seq.filter valid2 |> Seq.length)
