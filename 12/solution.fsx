let clampAngle angle =
    match angle with
    | _ when angle < 0 -> angle + 360
    | _ when angle >= 360 -> angle - 360
    | _ -> angle

type Heading =
    | North
    | East
    | South
    | West
with
    member this.Angle =
        match this with
        | North -> 0
        | East -> 90
        | South -> 180
        | West -> 270

    static member FromAngle angle =
        match clampAngle angle with
        | 0 -> North
        | 90 -> East
        | 180 -> South
        | 270 -> West
        | _ -> failwithf "Invalid angle: %i" angle

    static member (+) (heading : Heading, angle) =
        Heading.FromAngle(heading.Angle + angle)

let rotate (x, y) angle =
    match clampAngle angle with
    | 0 -> x, y
    | 90 -> -y, x
    | 180 -> -x, -y
    | 270 -> y, -x
    | _ -> failwithf "Invalid angle: %i" angle

type Command =
    | Slide of Heading
    | Forward
    | Turn

type Ship =
    { Heading : Heading
      Position : int * int
      Waypoint : int * int }

let input =
    sprintf "%s/input.txt" __SOURCE_DIRECTORY__
    |> System.IO.File.ReadAllLines
    |> Array.map (fun cmd ->
        let value = cmd.Substring(1) |> int
        match cmd.[0] with
        | 'N' -> Slide North, value
        | 'E' -> Slide East, value
        | 'S' -> Slide South, value
        | 'W' -> Slide West, value
        | 'F' -> Forward, value
        | 'L' -> Turn, -value
        | 'R' -> Turn, value
        | _ -> failwithf "Bad command: %s" cmd)
    |> List.ofArray

let moveByHeading heading (x, y) amount =
    match heading with
    | North -> x, y-amount
    | East -> x+amount, y
    | South -> x, y+amount
    | West -> x-amount, y

let doCommand1 (command, value) ship =
    match command with
    | Slide heading -> { ship with Position = moveByHeading heading ship.Position value }
    | Forward -> { ship with Position = moveByHeading ship.Heading ship.Position value }
    | Turn -> { ship with Heading = ship.Heading + value}

let doCommand2 (command, value) ship =
    match command with
    | Slide heading -> { ship with Waypoint = moveByHeading heading ship.Waypoint value }
    | Forward ->
        let x, y = ship.Position
        let dx, dy = ship.Waypoint
        { ship with Position = x+dx*value, y+dy*value }
    | Turn -> { ship with Waypoint = rotate ship.Waypoint value}

let rec doCommands doCommand commands ship =
    match commands with
    | command :: tail ->
        doCommands doCommand tail (doCommand command ship)
    | [] -> ship

let shipStart =
    { Heading = East
      Position = 0, 0
      Waypoint = 10, -1 }

let shipEnd1 =
    shipStart |> doCommands doCommand1 input

let shipEnd2 =
    shipStart |> doCommands doCommand2 input

let manhattan (x, y) =
    (abs x) + (abs y)

printfn "Part 1 answer: %i" (manhattan shipEnd1.Position)
printfn "Part 2 answer: %i" (manhattan shipEnd2.Position)
