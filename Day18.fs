module Day18

open System
open System.Globalization
open System.Text.RegularExpressions

type Dir = R | D | L | U

let parseLines useColour (lines: string list) =
    let parseLine (line: string) =
        let mtch = Regex.Match(line, @"(\w) (\d+) \(#(\w+)\)")
        if not useColour then
            let dir =
                match mtch.Groups[1].Value[0] with
                | 'R' -> R | 'D' -> D | 'L' -> L | 'U' -> U
            (dir, (mtch.Groups[2].Value |> int64))
        else
            let hex = mtch.Groups[3].Value
            let distTxt, dirTxt = hex[..^1], hex[^0]
            let dist = Int64.Parse(distTxt, NumberStyles.HexNumber)
            let dir =
                match dirTxt with | '0' -> R | '1' -> D | '2' -> L | '3' -> U
            (dir, dist)
    lines |> List.map parseLine

let addEdge ((x, y), (prevPrevDir, prevDir, prevDist)) (dir, dist) =
    let (x', y') =
        match dir with
        | R -> (x + dist, y)
        | L -> (x - dist, y)
        | D -> (x, y + dist)
        | U -> (x, y - dist)

    let areaContrib =
        match prevPrevDir, prevDir, dir with

        | D, R, D -> (x + 1L) * (dist + 1L) - (x + 1L - prevDist)
        | U, R, D -> (x + 1L) * (dist + 1L)
        | D, L, D -> (x + 1L) * (dist + 1L) - (x + 1L)
        | U, L, D -> (x + 1L) * (dist + 1L) + (prevDist - 1L)

        | U, L, U -> -(x * (dist + 1L)) + (x + prevDist)
        | D, L, U -> -(x * (dist + 1L))
        | U, R, U -> -(x * (dist + 1L)) + x
        | D, R, U -> -(x * (dist + 1L)) + (prevDist - 1L)

        | _, _, L -> 0L
        | _, _, R -> 0L
        | _ -> failwith "oops"

    (areaContrib, ((x', y'), (prevDir, dir, dist)))

let calcArea useColour =
    parseLines useColour
    >> List.mapFold addEdge ((0L, 0L), (L, U, 0L))
    >> fst
    >> List.sum

let part1 getLines = getLines "input" |> calcArea false
let part2 getLines = getLines "input" |> calcArea true
