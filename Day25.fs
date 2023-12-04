module Day25

open System
open System.Text.RegularExpressions

let parseLine (line: string) =
    line

let part1 (getLines: string -> string list) =
    getLines "test1"
    |> List.map parseLine
    |> (fun (x) -> String.Join(", ", x))

let part2 (getLines: string -> string list) =
    getLines "input"
    |> List.length
