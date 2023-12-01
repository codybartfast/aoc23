﻿open System
open System.IO

let inputFile = Path.Combine("../../../input/2023", "day01", "input.txt")
let lines = File.ReadAllLines(inputFile) |> List.ofArray

let parseLine (line: string) = line;

let items = lines |> List.map parseLine

printfn "%A" items
