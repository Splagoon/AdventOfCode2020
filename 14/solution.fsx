open System.Text.RegularExpressions

type State =
    { Mask : bool option array
      Memory : Map<uint64, uint64> }

let toBits x =
    seq {
        for i in 35..(-1)..0 ->
            ((x >>> i) &&& 1uL) = 1uL
    } |> Array.ofSeq

let fromBits x =
    let revBits = x |> Array.rev
    let rec reconstruct bit value =
        if bit > 35
        then value
        else
            let bitVal =
                if revBits.[bit]
                then 1uL <<< bit
                else 0uL
            reconstruct (bit+1) (value+bitVal)
    reconstruct 0 0uL

let applyMask1 (mask : bool option array) value =
    value
    |> toBits
    |> Array.mapi (fun i b ->
        match mask.[i] with
        | Some b -> b
        | None -> b)
    |> fromBits

let applyMask2 (mask : bool option array) value =
    let patch index value arr =
        arr |> Array.mapi (fun i v -> if i = index then value else v)

    let rec apply bit values =
        if bit > 35
        then values
        else
            match mask.[bit] with
            | Some false -> apply (bit+1) values
            | Some true -> apply (bit+1) (values |> List.map (patch bit true))
            | None -> apply (bit+1) ((values |> List.map (patch bit false)) @ (values |> List.map (patch bit true)))

    apply 0 [toBits value]
    |> List.map fromBits

type Instruction =
    | SetMask of bool option array
    | SetMemory of uint64 * uint64

let maskRegex = Regex("mask = ([01X]{36})", RegexOptions.Compiled)
let memRegex = Regex("mem\[(\d+)\] = (\d+)")

let parseMask str =
    str
    |> Seq.map (function
        | '0' -> Some false
        | '1' -> Some true
        | 'X' -> None
        | c -> failwithf "Bad character in mask: %c" c)
    |> Array.ofSeq

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        match maskRegex.Match(line), memRegex.Match(line) with
        | m, _ when m.Success -> SetMask (parseMask m.Groups.[1].Value)
        | _, m when m.Success -> SetMemory (uint64 m.Groups.[1].Value, uint64 m.Groups.[2].Value)
        | _ -> failwithf "Bad instruction: %s" line)
    |> List.ofArray

let zeroState =
    { Mask = Array.init 36 (fun _ -> None)
      Memory = Map.empty }

let rec execute addressMask valueMask instructions state =
    match instructions with
    | (SetMask newMask) :: tail ->
        execute addressMask valueMask tail { state with Mask = newMask }
    | (SetMemory (address, value)) :: tail ->
        let maskedAddresses : uint64 list = address |> addressMask state.Mask
        let maskedValue = value |> valueMask state.Mask
        let newMemory =
            maskedAddresses
            |> List.fold (fun map key -> Map.add key maskedValue map) state.Memory
        execute addressMask valueMask tail { state with Memory = newMemory }
    | [] -> state

let execute1 = execute (fun _ -> List.singleton) applyMask1
let execute2 = execute applyMask2 (fun _ -> id)

let sumMemory state =
    state.Memory |> Map.toSeq |> Seq.sumBy snd

printfn "Part 1 answer: %i" (execute1 input zeroState |> sumMemory)
printfn "Part 2 answer: %i" (execute2 input zeroState |> sumMemory)
