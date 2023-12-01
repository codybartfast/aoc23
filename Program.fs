open System.IO

open Day01
let day = "01"

let getLines day file =
    let inputFile file = Path.Combine("../../../input/2023", $"day{day}", $"{file}.txt")
    File.ReadAllLines(inputFile file) |> List.ofArray

[<EntryPoint>]
let main _ =
    part1 (getLines day)
    part2 (getLines day)
    0
