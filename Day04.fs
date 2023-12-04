module Day04

open System.Text.RegularExpressions

let matchCount line =
    let [| _;  winners; held |] =
        Regex.Split(line, @"\s*[:\|]\s+")
        |> Array.map(fun txt -> Regex.Split(txt, @"\s+"))
    Set.count <| Set.intersect (Set winners) (Set held)

let part1 getLines =
    getLines "input" |> List.map matchCount
    |> List.sumBy (function 0 -> 0 | mc -> 1 <<< (mc - 1))

let part2 getLines =
    let matchCounts = getLines "input" |> List.map matchCount |> Array.ofList
    let cardCounts = Array.create matchCounts.Length 1

    (cardCounts, matchCounts) ||> Array.iteri2 (fun i nCards nMatches ->
        [i + 1 .. i + nMatches]
        |> List.iter(fun j -> cardCounts[j] <- cardCounts[j] + nCards))

    cardCounts |> Array.sum
