module Day03

open System
open System.Text.RegularExpressions

type Number = {
    Coord: (int * int)
    Value: int
    Digits: char list
    TopLeft: (int * int)
    BottomRight: (int * int)
}

let grid (lines : string list) =
    let lines = lines |> List.map (fun ln -> ln.ToCharArray()) |> Array.ofList
    [for y in 0 .. (lines.Length - 1) do
         for x in 0 .. (lines[0].Length - 1) do
             yield ((x, y), lines[y][x])]
    |> List.filter (snd >> ((<>) '.'))
    |> Map

let numbers (grid: Map<(int * int),char>) =
    let toNum digits (exX, exY) =
        match digits with
        | [] -> None
        | digits ->
            let coord = (exX - digits.Length, exY)
            let digits = List.rev digits
            let value = digits |> Array.ofList |> String |> Int32.Parse
            Some {
                Coord = (exX - digits.Length, exY)
                Digits = digits
                Value = value
                TopLeft =  (fst coord - 1, exY - 1)
                BottomRight = (exX, exY + 1)
            }
    let rec numbers digits expected squares = seq{
        match squares, digits with
        | [], [] -> yield None
        | [], digits -> yield toNum digits expected
        | ((x, y), c)::sqs, [] when Char.IsNumber(c) ->
            yield! numbers [c] (x + 1, y) sqs
        | ((x, y), c)::sqs, [] ->
            yield! numbers [] (x + 1, y) sqs
        | ((x, y), c)::sqs, _ when Char.IsNumber(c) && (x, y) = expected  ->
            yield! numbers (c::digits) (x + 1, y) sqs
        | ((x, y), c)::sqs, _ ->
            yield toNum digits expected
            yield! numbers [] (x, y) squares
    }
    grid |> Map.toSeq |> Seq.sortBy (fun ((x, y), _) -> (y, x)) |> List.ofSeq
    |> numbers [] (0, 0)
    |> Seq.choose id
    |> List.ofSeq

let symbols grid =
    grid
    |> Map.toSeq
    |> Seq.filter (snd >> Char.IsNumber >> not)
    |> List.ofSeq

let touches number symbol =
    let (x, y), _ = symbol
    let {TopLeft = (lx, ty); BottomRight = (rx, by)} = number
    (lx <= x && x <= rx) && (ty <= y && y <= by)



let part1 getLines =
    let grid =  getLines "input" |> grid
    let symbols =
        grid
        |> Map.toSeq |> Seq.filter (snd >> Char.IsNumber >> not) |> List.ofSeq
    let numbers = grid |> numbers
    let rslt =
        numbers
        |> List.filter (fun num -> List.exists (touches num) symbols )
        |> List.sumBy (fun num -> num.Value)
    // rslt |> List.iter (printfn "%O")
    rslt

let part2 getLines  =
    let grid =  getLines "input" |> grid
    let symbols =
        grid
        |> Map.toSeq |> Seq.filter (snd >> Char.IsNumber >> not) |> List.ofSeq
    let stars = symbols |> List.filter (snd >> ((=) '*'))
    let numbers = grid |> numbers
    let gears =
        stars
        |> List.map (fun star -> numbers |> List.filter(fun num -> touches num star ))
        |> List.filter (List.length >> ((=) 2))
        |> List.sumBy (List.map (fun num -> num.Value) >> List.reduce  (*))


    // rslt |> List.iter (printfn "%O")
    gears
