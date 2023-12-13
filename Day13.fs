module Day13

let parseLines lines =
    let parseLine (line: string) = line.ToCharArray() |> List.ofArray
    lines |> List.map parseLine

let splitOn sep =
    let rec split part parts = function
        | [] -> ((part |> List.rev) :: parts) |> List.rev
        | h :: t when h  = sep -> split [] ((part |> List.rev) :: parts) t
        | h :: t -> split (h :: part) parts t
    split [] []

let rec hasSmudgeCount count top bottom =
    (top |> Seq.collect id, bottom |> Seq.collect id)
    ||> Seq.zip
    |> Seq.map (fun (a, b) -> a <> b)
    |> Seq.scan (fun count smudged -> if smudged then count + 1 else count) 0
    |> Seq.takeWhile (fun n -> n <= (count + 1))
    |> Seq.last
    |> ((=) count)

let findRow smudgeCount (pattern: char list list) =
    [1 .. pattern.Length - 1]
    |> List.choose (fun i ->
        let (top, bottom) = List.splitAt i pattern
        let len = min top.Length bottom.Length
        let topPart = top |> List.rev |> List.take len
        let bottomPart = bottom |> List.take len
        if hasSmudgeCount smudgeCount topPart bottomPart then Some i else None)
    |> function [] -> 0 | [n] -> n

let findMirrors smudgeCount pattern =
    (pattern |> List.transpose |> findRow smudgeCount,
        pattern |> findRow smudgeCount)

let summerise smudgeCount =
    List.map (findMirrors smudgeCount)
    >> List.sumBy (fun (a, b) ->  a + b * 100)

let part1 getLines =
    "input" |> getLines |> parseLines |> splitOn [] |> summerise 0

let part2 getLines =
    "input" |> getLines |> parseLines |> splitOn [] |> summerise 1
