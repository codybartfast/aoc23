open System
open System.IO

let inputFile = Path.Combine("../../../input/2023", "day01", "input.txt")
let lines = File.ReadAllLines(inputFile)

let parseLine line = line

let items = lines |> Array.map parseLine

printfn $"""{String.concat ", " items}"""
