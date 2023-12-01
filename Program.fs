open System.IO

open Day02
let day = "02"

let getLines day file =
    Path.Combine("../../../input/2023", $"day{day}", $"{file}.txt")
    |> File.ReadAllLines
    |> List.ofArray

[<EntryPoint>]
let main _ =
    part1 (getLines day)
    part2 (getLines day)
    0
