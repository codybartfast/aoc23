module Day09

open System
open System.Text.RegularExpressions

let parseLines lines =
    let parseLine (line: string) =
        line.Split(' ') |> Array.map int |> List.ofArray
    lines |> List.map parseLine

let rec nextValue (readings: int list) : int =
    if readings |> List.forall ((=) 0) then 0 else
        let diffs = readings |> List.pairwise |> List.map (fun (a, b) -> b - a)
        (readings |> List.last) + (nextValue diffs)

let part1 (getLines: string -> string list) =
    getLines "input"
    |> parseLines
    |> List.map nextValue
    |> List.sum

let part2 (getLines: string -> string list) =
    getLines "input"
    |> parseLines
    |> List.map (List.rev >> nextValue)
    |> List.sum
