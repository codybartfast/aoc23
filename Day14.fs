module Day14

let rotateRight = Array.transpose >> Array.map Array.rev

let rotateLeft = Array.map Array.rev >> Array.transpose

(*  Orientate the platform with North on the 'left' so that stones roll
    within a row  *)
let parseLines lines =
    let parseLine (line: string) = line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList
    |> rotateLeft

let tiltLeft platform =
    let rec findRest (row: char[]) i =
        let nxt = i - 1
        if nxt >= 0 && row[nxt] = '.' then findRest row nxt else i
    let tiltRowLeft (row: char []) =
        row |> Array.iteri (fun i c ->
            if c = 'O' then
                let dest = findRest row i
                row[i] <- '.'
                row[dest] <- 'O')
        row
    platform |> Array.map (Array.copy >> tiltRowLeft)

let rec repeat func n value =
    if n = 0 then value else repeat func (n - 1) (func value)

let cycle = repeat (tiltLeft >> rotateRight) 4

let totalLoad =
    rotateLeft
    >> Array.mapi (fun i row ->
        (i + 1) * (row |> Array.filter ((=) 'O') |> Array.length))
    >> Array.sum

let rec findDuplicate nCycles known platform  =
    let key = (platform |> totalLoad) * (platform |> rotateLeft |> totalLoad)
    match Map.tryFind key known with
    | Some n ->  (n, nCycles), platform
    | _ -> findDuplicate
            (nCycles + 1)
            (known |> Map.add key nCycles)
            (cycle platform)

let part1 getLines =
    "input" |> getLines |> parseLines
    |> tiltLeft
    |> totalLoad

let part2 getLines =
    let platform = "input" |> getLines |> parseLines
    let (fstCycle, dupCycle), dupPlat = platform |> findDuplicate 0 Map.empty
    let remaining = (1_000_000_000 - dupCycle) % (dupCycle - fstCycle)
    repeat cycle remaining dupPlat
    |> totalLoad
