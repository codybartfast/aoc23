module Day08

open System
open System.Text.RegularExpressions

let parseLines (lines: string list) =
    let parseLine ln =
        Regex("\w+").Matches(ln) |> Seq.map (fun m -> m.Value) |> List.ofSeq
        |> (fun [name; left; right] -> (name, (left, right)))
    let top::_::rest = lines
    let directions = top.ToCharArray() |> List.ofArray
    let nodes = rest |> List.map parseLine |> Map
    (directions, nodes)

let navigate start (allDirections, nodes) =
    let rec nav current dirs count =
        match current, dirs with
        | name, _  when (name: string)[2] = 'Z' -> count
        | _, [] -> nav current allDirections count
        | _, dir::rest ->
            match dir, Map.find current nodes with
            | 'L', (next, _)
            | 'R', (_, next) -> nav next rest (count + 1)
    nav start allDirections 0

let rec lcm x y =
    let rec hcf a b = if b = 0UL then a else hcf b (a % b)
    x * (y / (hcf x y))

let part1 getLines = getLines "input" |> parseLines |> navigate "AAA"

let part2 getLines =
    let (_, nodes) as input = getLines "input" |> parseLines

    nodes
    |> Map.toList
    |> List.map fst
    |> List.filter (fun name -> name[2] = 'A')
    |> List.map (fun start -> navigate start input |> Convert.ToUInt64)
    |> List.reduce lcm
