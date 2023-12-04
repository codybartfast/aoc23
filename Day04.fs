module Day04

open System

let parseLine (line: string) =
    let [| head ; numbers |] = line.Split(':')
    let id = head.Split(' ').[1]
    let [|winTxt; heldTxt |] = numbers.Split('|')
    let toNums (txt: string) =
        txt.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun t -> t.Trim() |> int)
        |> List.ofArray
    (toNums winTxt, toNums heldTxt)

let countMatches (winners: int list, held: int list) =
    held |> List.filter (fun h -> List.contains h winners) |> List.length

let score numbers =
    numbers |> countMatches |> (function | 0 -> 0 | n -> 1 <<< (n - 1))

let part1 getLines = getLines "input" |> List.map parseLine |> List.sumBy score

let part2 (getLines: string -> string list) =
    let lines = getLines "input"
    let matches = lines |> List.map (parseLine >> countMatches) |> Array.ofList
    let cards = Array.init matches.Length (fun _ -> 1)

    (cards, matches) ||> Array.iteri2 (fun i cCount mCount ->
        [i + 1 .. i + mCount]
        |> List.iter(fun j -> cards.[j] <- cards.[j] + cCount))

    cards |> Array.sum
