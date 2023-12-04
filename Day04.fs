module Day04

open System.Text.RegularExpressions

let toCard line =
    let [| _ ; winTxt ; heldTxt |] = Regex.Split(line, @"\s*[:\|]\s+")
    let toNums (txt: string) =
        Regex.Split(txt, @"\s+") |> Array.map int |> List.ofArray
    (toNums winTxt, toNums heldTxt)

let matchCounts lines =
    let countMatches (winners, held) =
        held |> List.filter (fun h -> List.contains h winners) |> List.length
    lines |> List.map (toCard >> countMatches)

let part1 getLines =
    getLines "input"
    |> matchCounts
    |> List.sumBy (function 0 -> 0 | mc -> 1 <<< (mc - 1))

let part2 getLines =
    let matchCounts = getLines "input" |> matchCounts |> Array.ofList
    let cardCounts = Array.init matchCounts.Length (fun _ -> 1)

    (cardCounts, matchCounts) ||> Array.iteri2 (fun i nCards nMatches ->
        [i + 1 .. i + nMatches]
        |> List.iter(fun j -> cardCounts[j] <- cardCounts[j] + nCards))

    cardCounts |> Array.sum
