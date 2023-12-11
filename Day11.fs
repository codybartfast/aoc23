module Day11

open System

let parseLines lines =
    let parseLine (line: string) = line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList

let expandedCoords factor (chart: char [] []) =
    let observedCoords =
        [ for y in 0 .. (chart.Length - 1) do
          for x in 0 .. (chart[0].Length - 1) do
              match chart[y][x] with
              | '#' -> Some(x, y)
              | _ -> None ]
        |> List.choose id
    let width, height = chart[0].Length, chart.Length

    let occupiedRows = observedCoords |> List.map snd |> Set
    let emptyRows =
        Set.difference ([ 0 .. height - 1 ] |> Set) occupiedRows |> List.ofSeq
    let emptyRowsBefore row = emptyRows |> List.filter ((>) row) |> List.length

    let occupiedCols = observedCoords |> List.map fst |> Set
    let emptyCols =
        Set.difference ([ 0 .. width - 1 ] |> Set) occupiedCols |> List.ofSeq
    let emptyColsBefore col = emptyCols |> List.filter ((>) col) |> List.length

    observedCoords
    |> List.map (fun (x, y) ->
        (x + (factor - 1) * (emptyColsBefore x))
        , (y + (factor - 1) * (emptyRowsBefore y)))

let sumDistances coords =
    let distance (x, y) (x', y': int) =
        abs (x - x') + abs (y - y') |> Convert.ToInt64
    let rec sum crds acc =
        match crds with
        | [] -> acc
        | g::gs -> sum gs (acc + (gs |> List.sumBy (distance g)))
    sum coords 0

let part1 getLines =
    "input"
    |> getLines
    |> parseLines
    |> expandedCoords 2
    |> sumDistances

let part2 getLines =
    "input"
    |> getLines
    |> parseLines
    |> expandedCoords 1_000_000
    |> sumDistances
