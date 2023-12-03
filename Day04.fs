module Day04

open System
open System.Text.RegularExpressions

let parseLine (line: string) =
    line

let part1 getLines =
    getLines "test1"
    |> List.map parseLine
    |> (fun (x) -> String.Join(", ", x))

let part2 getLines =
    getLines "input"
    |> List.map parseLine
    |> List.length
