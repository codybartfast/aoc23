module Day06

open System
open System.Text.RegularExpressions

let records unkern (lines: string list) =
    let [times; dists] = lines|> List.map (fun ln ->
        Regex.Split(ln, @"\s+")[1..]
        |> List.ofArray
        |> fun parts -> if unkern then [String.concat "" parts] else parts
        |> List.map Int64.Parse)
    List.zip times dists

let shortestPress time dist =
    let rec search low guess high =
        match guess * (time - guess) > dist with
        | true when guess - low = 1L -> guess
        | true -> search low ((guess + low) / 2L) guess
        | false -> search guess ((guess + high + 1L) / 2L) high
    let guess = dist |> float |> sqrt |> Convert.ToInt64
    search 1 guess guess

let variations (time, dist) = 1L + (time - 2L * shortestPress time dist)

let part1 (getLines: string -> string list) =
    getLines "input"
    |> records false
    |> List.map variations
    |> List.reduce (*)

let part2 (getLines: string -> string list) =
    getLines "input"
    |> records true
    |> List.map variations
    |> List.reduce (*)
