module Day16

type Cave = char [][]
type Dir = N | E | S | W
type Beam = (int * int) * Dir
type Energized = (Dir list)[][]

let readCave (lines: string list) =
    lines |> List.map _.ToCharArray() |> Array.ofList

let nextBeam (cave: Cave) (((x,y), dir): Beam) =
    let x', y' =
        match dir with
        | N -> x, y - 1
        | E -> x + 1, y
        | S -> x, y + 1
        | W -> x - 1, y
    if x' < 0 || x' >= cave[0].Length || y' < 0 || y' >= cave.Length then
        None
    else
        Some ((x', y'), dir)

let nextDirections (tile: char) (curr: Dir)=
    match tile, curr with
    | '.', _ -> [curr]
    | '|', N | '|', S -> [curr]
    | '|', W | '|', E -> [N; S]
    | '-', W | '-', E -> [curr]
    | '-', N | '-', S -> [W; E]
    | '/', N -> [E]
    | '/', E -> [N]
    | '/', W -> [S]
    | '/', S -> [W]
    | '\\', N -> [W]
    | '\\', E -> [S]
    | '\\', S -> [E]
    | '\\', W -> [N]

let shine (cave: Cave) (mbeam: Beam option) (energized: Energized) : (Beam option) list =
    let rec shine mayBeam unfollowedBeams =
        match mayBeam with
        | None -> unfollowedBeams
        | Some ((x, y), dir) ->
        let prevDirs = energized[y][x]
        if prevDirs |> List.contains dir then
            unfollowedBeams
        else
            energized[y][x] <- (dir::prevDirs)
            let (nextDir::newDirs) = nextDirections (cave[y][x]) dir
            let unfollowedBeams' =
                (newDirs |> List.map (fun dr -> nextBeam cave ((x, y), dr)))
                    @ unfollowedBeams
            shine (nextBeam cave ((x, y), nextDir)) unfollowedBeams'
    shine mbeam []

let rec shineAll (cave: Cave) (unfollowedBeams: Beam option list) (energized: Energized) =
    match unfollowedBeams with
    | [] -> energized
    | beam::unfollowedBeams ->
        let newBeams = shine cave beam energized
        shineAll cave (newBeams @ unfollowedBeams) energized

let energizedCount  =
    Array.collect id >> Array.filter ((<>) [])  >> Array.length

let newEnergized (cave: Cave) =
    Array.init cave.Length (fun _ -> Array.create cave[0].Length [])

let starts (cave: Cave) =
    let maxX, maxY = cave[0].Length - 1, cave.Length - 1
    let ys, xs = [0 .. maxY], [0 .. maxX]
    let fromLeft = ys |> List.map (fun y -> ((0, y), E))
    let fromRight = ys |> List.map (fun y -> ((maxX, y), W))
    let fromTop = xs |> List.map (fun x -> ((x, 0), S))
    let fromBottom = xs |> List.map (fun x -> ((x, maxY), N))
    List.collect id [fromLeft; fromRight; fromTop; fromBottom]
    |> List.map Some

let part1 (getLines: string -> string list) =
    let cave = "input" |> getLines  |> readCave
    shineAll cave [Some ((0, 0), E)] (newEnergized cave)
    |> energizedCount

let part2 (getLines: string -> string list) =
    let cave = "input" |> getLines  |> readCave

    starts cave
    |> List.map (fun start ->
        energizedCount <| shineAll cave [start] (newEnergized cave))
    |> List.max
