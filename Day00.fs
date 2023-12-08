module Day00

open System
open System.Text.RegularExpressions

let parseLine (line: string) =
    let thing = Regex.Split(line, @"\s+") |> List.ofArray
    thing

let part1 (getLines: string -> string list) =
    getLines "test1"
    |> List.map parseLine
    |> fun x -> String.Join(" | ", x)

let part2 (getLines: string -> string list) =
    let lines = getLines "input"
    $"{lines.Length} lines: {lines}"
