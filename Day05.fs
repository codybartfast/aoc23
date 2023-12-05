module Day05

open System

let splitOn sep =
    let rec split part parts = function
        | [] -> ((part |> List.rev) :: parts) |> List.rev
        | h :: t when h  = sep -> split [] ((part |> List.rev) :: parts) t
        | h :: t -> split (h :: part) parts t
    split [] []

let seedSpans isRange (line: string) =
    line.Split(" ")[1..] |> List.ofArray |> List.map uint |>
        if isRange then
            List.chunkBySize 2 >> List.map (fun [a; b] -> (a, a + b))
        else
            List.map (fun a -> (a, a))

let toMapSpec (line: string) =
    line.Split(" ") |> Array.map uint
    |> (fun [| dStart; sStart; length |] -> ((sStart, sStart + length), dStart))

// pads the defined maps with with 'id' maps so all values of uint are covered
let fillMapSpecs mapSpec =
    let rec fill start filled = function
        | [] -> (((start, UInt32.MaxValue), start) :: filled) |> List.rev
        | ((srcStart, srcEnd), _) as spec::rest when start = srcStart  ->
            fill srcEnd (spec :: filled) rest
        | ((srcStart, srcEnd), _) as spec::rest when start < srcStart  ->
            let idRange = ((start, srcStart), start)
            fill srcEnd (spec :: idRange :: filled) rest
    fill 0u [] (mapSpec |> List.sortBy (fst >> fst))

let applyMap mapSpec (spnStart, spnEnd) =
    mapSpec
    |> List.skipWhile (fun ((_, srcEnd), _) -> srcEnd < spnStart)
    |> List.takeWhile (fun ((srcStart, _), _) -> srcStart < spnEnd)
    |> List.map (fun ((srcStart, srcEnd), dstStart) ->
        let shift = dstStart - srcStart
        (shift + max spnStart srcStart, shift + min spnEnd srcEnd))

let bestLocation isRange lines =
    let seedPart :: mapParts = lines |> splitOn ""
    let seedSpans = seedPart[0] |> seedSpans isRange
    let mapSpecs =
        mapParts |> List.map (List.tail >> List.map toMapSpec >> fillMapSpecs)
    (seedSpans, mapSpecs)
        ||> List.fold (fun spans spec -> spans |> List.collect (applyMap spec))
    |> List.sortBy fst |> List.head |> fst

let part1 getLines = getLines "input" |> bestLocation false
let part2 getLines = getLines "input" |> bestLocation true
