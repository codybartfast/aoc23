module Day05

open System
open System.Text.RegularExpressions

let paragraphs (text: string) =
    Regex.Split(text, @"\n\n") |> List.ofArray

let numbers (line: string) =
    line.Split(' ') |> List.ofArray |> List.tail |> List.map uint

let spanFromNumbers (line: string) =
    line |> numbers |> List.map (fun a -> (a, a))

let spanFromRange (line: string) =
    line |> numbers |> List.chunkBySize 2 |> List.map (fun [a; b] -> (a, a + b))

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

let applyMap (spnStart, spnEnd) mapSpec =
    mapSpec
    |> List.skipWhile (fun ((_, srcEnd), _) -> srcEnd < spnStart)
    |> List.takeWhile (fun ((srcStart, _), _) -> srcStart < spnEnd  )
    |> List.map (fun ((srcStart, srcEnd), dstStart) ->
        let shift = dstStart - srcStart
        (shift + max spnStart srcStart, shift + min spnEnd srcEnd))

let bestLocation isRange lines =
    let seeds :: mapParas = lines |> String.concat "\n" |> paragraphs
    let spans = seeds |> if isRange then spanFromRange else spanFromNumbers
    let mapSpecs =
        mapParas
        |> List.map (fun p -> p.Split("\n") |> List.ofArray |> List.tail)
        |> List.map (List.map toMapSpec >> fillMapSpec)
    (spans, mapSpecs) ||> List.fold (fun spans spec ->
        spans |> List.collect (fun span -> applyMap span spec))
    |> List.sortBy fst
    |> List.head
    |> fst

let part1 (getLines: string -> string list) =
     getLines "input" |> bestLocation false

let part2 (getLines: string -> string list) =
    getLines "input" |> bestLocation true
