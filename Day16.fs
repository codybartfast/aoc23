module Day16

open System
open System.Text.RegularExpressions

type Cave = char [][]
type Dir = N | E | S | W
type Beam = (int * int) * Dir
type Energized = (Dir list)[][]

let readCave (lines: string list) =
    lines |> List.map _.ToCharArray() |> Array.ofList

let display (energized: Energized) (((x, y), dir): Beam) (otherBeams: (Beam option) list) =
    printfn ""
    let lensArray =
        energized
        |> Array.map(fun row ->
                row
                |> Array.map (fun dirList -> dirList.Length.ToString()))
    lensArray[y][x] <- dir.ToString()
    lensArray
    |> Array.iter(fun row ->
        row |> String.concat "" |> _.Replace("0", ".") |> printfn "%s")
    printfn ""

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
    | '|', N
    | '|', S -> [curr]
    | '|', W
    | '|', E -> [N; S]
    | '-', W
    | '-', E -> [curr]
    | '-', N
    | '-', S -> [W; E]
    | '/', N -> [E]
    | '/', E -> [N]
    | '/', W -> [S]
    | '/', S -> [W]
    | '\\', N -> [W]
    | '\\', E -> [S]
    | '\\', S -> [E]
    | '\\', W -> [N]

let shine (cave: Cave) (mbeam: Beam option) (energized: Energized) : (Beam option) list =
    let rec shine (mbeam: Beam option) (newBeams : (Beam option) list) : (Beam option) list =
        match mbeam with
        | None -> newBeams
        | Some ((x, y), dir) ->
        let prevDirs = energized[y][x]
        if energized[y][x] |> List.contains dir then
            newBeams
        else
            energized[y][x] <- (dir::prevDirs)
            let (nextDir::newDirs) = nextDirections (cave[y][x]) dir
            let newBeams' = (newDirs |> List.map (fun nd -> nextBeam cave ((x, y), nd))) @ newBeams
            shine (nextBeam cave ((x, y), nextDir)) newBeams'
    shine mbeam []

let rec shineAll (cave: Cave) (beams: Beam option list) (energized: Energized) =
    match beams with
    | [] -> energized
    | beam::beams' ->
        let newBeams = shine cave beam energized
        shineAll cave (newBeams @ beams') energized

let energizedCount (energized: Energized) =
    energized
    |> Array.collect id
    |> Array.filter ((<>) [])
    |> Array.length

let newEnergized (cave: Cave) =
    Array.init cave.Length (fun _ -> Array.create cave[0].Length [])

let starts (cave: Cave) =
    let maxX = cave[0].Length - 1
    let maxY = cave.Length - 1
    let ys = [0 .. maxY]
    let xs = [0 .. maxX]
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
        (shineAll cave [start] (newEnergized cave))
        |> energizedCount)
    |> List.max
