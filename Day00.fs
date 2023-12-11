module Day00

open System
open System.Text.RegularExpressions

let parseLines lines =
    let parseLine (line: string) =
        line.Split(' ') |> List.ofArray
    lines |> List.map parseLine

let part1 getLines =
    "test1" |> getLines  |> parseLines
    |> fun x -> String.Join(" | ", x)

let part2 getLines =
    let lines = "input" |> getLines
    $"{lines |> List.length} lines: {lines}"
