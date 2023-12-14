module Day14

type Platform = char [] []

let rotateRight (platform: Platform) =
    platform |> Array.transpose |> Array.map Array.rev

let rotateLeft (platform: Platform) =
    platform |> Array.map Array.rev |> Array.transpose


(*  The platform is orientated with North on the left so that stones roll
    within a row  *)
let parseLines lines =
    let parseLine (line: string) = line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList |> rotateLeft

let tiltLeft (platform: Platform) =
    let rec findRest (row: char[]) i =
        let nxt = i - 1
        if nxt >= 0 && row[nxt] = '.' then findRest row nxt else  i
    let tiltRowLeft (row: char []) =
        let row = Array.copy row
        row |> Array.iteri (fun i c ->
            if c = 'O' then
                let dest = findRest row i
                row[i] <- '.'
                row[dest] <- 'O')
        row
    platform |> Array.map tiltRowLeft

let rec repeat func n value =
    if n = 0 then value else repeat func (n - 1) (func value)

let cycle  = repeat (tiltLeft >> rotateRight) 4

let rec findDuplicate (nCycles: int) known (platform: Platform)  =
    let key = platform
    match Map.tryFind key known with
    | Some n ->
        (n, nCycles), platform
    | _ ->
        let known = known |> Map.add key nCycles
        findDuplicate (nCycles + 1) known (cycle platform)

let totalLoad (platform: Platform) =
    platform
    |> rotateLeft
    |> Array.mapi (fun i row ->
        (i + 1) * (row |> Array.filter ((=) 'O') |> Array.length))
    |> Array.sum

let part1 getLines =
    "input" |> getLines  |> parseLines
    |> tiltLeft
    |> totalLoad

let part2 getLines =
    let (first, second), platform =
        "input" |> getLines  |> parseLines
        |> findDuplicate 0 (Map.empty<Platform, int>)
    let remaining = (1_000_000_000 - second) % (second - first)
    repeat cycle remaining platform
    |> totalLoad
