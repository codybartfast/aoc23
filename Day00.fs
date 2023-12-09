module Day00

open System
open System.Text.RegularExpressions

let parseLines lines =
    let parseLine (line: string) =
        line.Split(' ') |> List.ofArray
    lines |> List.map parseLine

let part1 (getLines: string -> string list) =
    getLines "test1"
    |> parseLines
    |> fun x -> String.Join(" | ", x)

let part2 (getLines: string -> string list) =
    let lines = getLines "input"
    $"{lines.Length} lines: {lines}"
