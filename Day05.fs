module Day05

open System
open System.Text.RegularExpressions

let paragraphs (lines: string list) : string list list =
    let rec paragraphs part paras = function
        | [] -> ((part |> List.rev) :: paras) |> List.rev
        | ("" :: rest) -> paragraphs [] ((part |> List.rev) :: paras) rest
        | ln :: rest -> paragraphs (ln :: part) paras rest
    paragraphs  [] [] lines

let seedVals (line: string) =
    line.Split(' ') |> List.ofArray |> List.tail |> List.map uint

let spanFromNumber (line: string) =
    line |> seedVals |> List.map (fun a -> (a, a))

let spanFromRange (line: string) =
    line |> seedVals |> List.chunkBySize 2 |> List.map (fun [a; b] -> (a, a + b))

let toMapSpec (line: string) =
    Regex.Split(line, "\s+") |> Array.map uint
    |> (fun [| dStart; sStart; length |] -> ((sStart, sStart + length), dStart))

let fillMapSpec (mapSpec: ((uint * uint) * uint) list) =
    let rec fill start specs filled =
        match specs with
        | [] -> (((start, UInt32.MaxValue), start) :: filled) |> List.rev
        | ((srcStart, srcEnd), _) as spec::rest when start = srcStart  ->
            fill srcEnd rest (spec :: filled)
        | ((srcStart, srcEnd), _) as spec::rest when start < srcStart  ->
            let idRange = ((start, srcStart), start)
            fill srcEnd rest (spec :: idRange :: filled)
    fill 0u (mapSpec |> List.sortBy (fst >> fst)) []

let applyMap mapSpec (spnStart, spnEnd) =
    mapSpec
    |> List.skipWhile (fun ((_, srcEnd), _) -> srcEnd < spnStart)
    |> List.takeWhile (fun ((srcStart, _), _) -> srcStart < spnEnd  )
    |> List.map (fun ((srcStart, srcEnd), dstStart) ->
        let shift = dstStart - srcStart
        (shift + max spnStart srcStart, shift + min spnEnd srcEnd))

let bestLocation isRange lines =
    let seedPara :: mapParas = lines |> paragraphs
    let seedSpans =
        seedPara[0]  |> if isRange then spanFromRange else spanFromNumber
    let mapSpecs =
        mapParas |> List.map (List.tail >> List.map toMapSpec >> fillMapSpec)
    (seedSpans, mapSpecs) ||> List.fold (fun spans spec ->
        spans |> List.collect (applyMap spec))
    |> List.sortBy fst |> List.head |> fst

let part1 (getLines: string -> string list) =
     getLines "input" |> bestLocation false

let part2 (getLines: string -> string list) =
    getLines "input" |> bestLocation true
