module Day07

open System
open System.Text.RegularExpressions

let parseLine wildcards (line: string) =
    let [| cardsTxt; bidTxt |] = Regex.Split(line, @"\s+")
    let cards = cardsTxt.ToCharArray() |> List.ofArray |> List.map (function
        | 'J' -> if wildcards then '0' else 'j'
        | 'Q' -> 'q'
        | 'K' -> 'r' // rex
        | 'A' -> 'z'
        | c -> c )
    (cards, int bidTxt)

let handType (hand: Char list, _) =
    let (jokers, rest) = hand |> List.partition ((=) '0')
    let lengths =
        rest
        |> List.countBy id
        |> List.map snd
        |> List.sortDescending

    match lengths with
    | [_] | [] -> 6 // Five of a kind
    | longest::nextLongest::_ ->
        match jokers.Length, longest, nextLongest with
        | 0, 4, _ -> 5 // Four of a kind
        | 0, 3, 2 -> 4 // Full house
        | 0, 3, _ -> 3 // Three of a kind
        | 0, 2, 2 -> 2 // Two pair
        | 0, 2, _ -> 1 // One Pair
        | 0, _, _ -> 0 // High card

        | 1, 4, _ -> 6
        | 1, 3, _ -> 5
        | 1, 2, 2 -> 4
        | 1, 2, _ -> 3
        | 1, _, _ -> 1

        | 2, 3, _ -> 6
        | 2, 2, _ -> 5
        | 2, _, _ -> 3

        | 3, 2, _ -> 6
        | 3, _, _ -> 5

        | _ -> 6

let winnings =
    List.map (fun hand -> (handType hand, hand))
    >> List.sort
    >> List.mapi (fun i (_, (_, bid)) -> (i + 1) * bid)
    >> List.sum

let part1 getLines = getLines "input" |> List.map (parseLine false) |> winnings

let part2 getLines = getLines "input" |> List.map (parseLine true) |> winnings
