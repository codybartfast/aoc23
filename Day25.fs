module Day25

open System
open System.Text.RegularExpressions

let part1 getLines =
    getLines "test1"
    |> printfn "Part One: %A"

let part2 getLines =
    getLines "input"
    |> List.length
    |> printfn "Part Two: %A"
