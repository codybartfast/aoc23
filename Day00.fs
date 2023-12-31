module Day00

open System
open System.Text.RegularExpressions

let parseLines (lines: string list) =
    let parseLine (line: string) =
        line
    lines |> List.map parseLine

let part1 (getLines: string -> string list) =
    "test1" |> getLines  |> parseLines
    |> fun x -> String.Join(" | ", x)

let part2 (getLines: string -> string list) =
    "input" |> getLines  |> Seq.length
    |> fun x -> String.Join(" | ", x)
