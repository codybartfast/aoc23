module Day00

open System
open System.Text.RegularExpressions

let parseLines (lines: string list) =
    let parseLine (line: string) =
        line
        // line.Split(' ') |> List.ofArray
    lines |> List.map parseLine

let part1 (getLines: string -> string list) =
    "test1" |> getLines  |> parseLines
    |> fun x -> String.Join(" | ", x)

let part2 (getLines: string -> string list) =
    "input" |> getLines  |> parseLines
    |> fun x -> String.Join(" | ", x)
