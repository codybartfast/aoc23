module Day12

open System

type Known = Map<string * int list, int64>

let parseLines lines =
    let parseLine (line: string) =
        let [|springTxt; recordTxt|] = line.Split(' ')
        (springTxt.ToCharArray() |> List.ofArray,
            recordTxt.Split(',') |> Array.map int |> List.ofArray)
    lines |> List.map parseLine

let rec fits springs entry =
    match entry, springs with
    | 0, [] -> Some []
    | 0, ('.' :: rest)
    | 0, ('?' :: rest) -> Some rest
    | _, ('?'::sprs)
    | _, ('#'::sprs) -> fits sprs (entry - 1)
    | _ -> None

let rec arrangements springs record known : (int64 * Known) =
    let key = (springs |> Array.ofList |> String, record)
    match Map.tryFind key known with
    | Some n -> (n, known)
    | None ->
        let (rslt: int64, known) =
            match springs, record with
            | _, [] when springs |> List.forall (fun c -> c = '.' || c = '?') ->
                (1L, known)
            | [], _ | _, [] -> (0L, known)
            | (spring::rstSprings), (entry::rstRecords) ->
                let (useCount, known) =
                    match fits springs entry with
                    | Some sprs -> arrangements sprs rstRecords known
                    | None -> (0, known)
                let (skipCount, known)  =
                    match spring with
                    | '#' -> (0L, known)
                    | _ -> arrangements rstSprings record known
                (useCount + skipCount, known)
        (rslt, Map.add key rslt known)

let countArrangements (springs, record) : int64 =
    arrangements springs record (Map.empty: Known)
    |> fst

let unfold (springs, record) =
    let springs5 =
        '?'::springs |> List.replicate 5 |> List.collect id |> List.tail
    let record5 = record |> List.replicate 5 |> List.collect id
    (springs5, record5)

let part1 getLines =
    "input" |> getLines  |> parseLines
    |> List.sumBy countArrangements

let part2 getLines =
    "input" |> getLines  |> parseLines
    |> List.sumBy (unfold >> countArrangements)
