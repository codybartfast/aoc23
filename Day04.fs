module Day04

open System.Text.RegularExpressions

let toCard line =
    let [| _ ; winTxt ; heldTxt |] = Regex.Split(line, @"\s*[:\|]\s*")
    let toNums (txt: string) =
        Regex.Split(txt, @"\s+") |> Array.map int |> List.ofArray
    (toNums winTxt, toNums heldTxt)

let countMatches (winners, held) =
    held |> List.filter (fun h -> List.contains h winners) |> List.length

let score card =
    card |> countMatches |> (function | 0 -> 0 | n -> 1 <<< (n - 1))

let part1 getLines =
    getLines "input" |> List.map toCard |> List.sumBy score

let part2 getLines =
    let matchCounts =
        getLines "input" |> List.map (toCard >> countMatches) |> Array.ofList
    let cardCounts = Array.init matchCounts.Length (fun _ -> 1)

    (cardCounts, matchCounts) ||> Array.iteri2 (fun i nCards nMatches ->
        [i + 1 .. i + nMatches]
        |> List.iter(fun j -> cardCounts[j] <- cardCounts[j] + nCards))

    cardCounts |> Array.sum
