module Day05

open System

let paragraphs =
    let rec paragraphs part paras = function
        | [] -> ((part |> List.rev) :: paras) |> List.rev
        | ("" :: rest) -> paragraphs [] ((part |> List.rev) :: paras) rest
        | ln :: rest -> paragraphs (ln :: part) paras rest
    paragraphs  [] []

let seedVals (line: string) =
    line.Split(" ") |> List.ofArray |> List.tail |> List.map uint

let spanFromNumber =
    seedVals >> List.map (fun a -> (a, a))
let spanFromRange =
    seedVals >> List.chunkBySize 2 >> List.map (fun [a; b] -> (a, a + b))

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
    |> List.takeWhile (fun ((srcStart, _), _) -> srcStart < spnEnd  )
    |> List.map (fun ((srcStart, srcEnd), dstStart) ->
        let shift = dstStart - srcStart
        (shift + max spnStart srcStart, shift + min spnEnd srcEnd))

let bestLocation isRange lines =
    let seedParas :: mapParas = lines |> paragraphs
    let seedSpans =
        seedParas[0] |> if isRange then spanFromRange else spanFromNumber
    let mapSpecs =
        mapParas |> List.map (List.tail >> List.map toMapSpec >> fillMapSpecs)
    (seedSpans, mapSpecs)
        ||> List.fold (fun spans spec -> spans |> List.collect (applyMap spec))
    |> List.sortBy fst |> List.head |> fst

let part1 getLines = getLines "input" |> bestLocation false
let part2 getLines = getLines "input" |> bestLocation true
