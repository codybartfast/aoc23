module Day01

open System
open System.Text.RegularExpressions

let wordToDigit rightToLeft (txt: string)  =
    let pattern = "one|two|three|four|five|six|seven|eight|nine"
    let eval (m: Match) =
        match m.Value with
        | "one" -> "1"
        | "two" -> "2"
        | "three" -> "3"
        | "four" -> "4"
        | "five" -> "5"
        | "six" -> "6"
        | "seven" -> "7"
        | "eight" -> "8"
        | "nine" -> "9"
        | _ -> failwith "oops"
    let options =
        if rightToLeft then RegexOptions.RightToLeft else RegexOptions.None
    Regex(pattern, options).Replace(txt, eval, 1)

let calVal replaceFirst replaceLast text =
    [
        text |> replaceFirst |> Seq.find(Char.IsNumber)
        text |> replaceLast |> Seq.findBack(Char.IsNumber)
    ]
    |> String.Concat
    |> int

let sumCalVals firstReplace lastReplace =
    List.map (calVal firstReplace lastReplace)
    >> List.sum

let part1 getLines =
    getLines "input"
    |> sumCalVals id id
    |> printfn "Part One: %O"

let part2 getLines =
    getLines "input"
    |> sumCalVals (wordToDigit false) (wordToDigit true)
    |> printfn "Part Two: %O"
