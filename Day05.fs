module Day05

open System
open System.Text.RegularExpressions

let paragraphs (text: string) =
    Regex.Split(text, @"\n\n") |> List.ofArray

let numbers (line: string) =
    line.Split(' ') |> List.ofArray |> List.tail |> List.map uint

let ranges (line: string) =
    line |> numbers |> List.chunkBySize 2 |> List.map (fun [a; b] -> (a, b))

let toMapSpec (line: string) =
    Regex.Split(line, "\s+") |> Array.map uint
    |> (fun [| dStart; sStart; length |] -> ((sStart, sStart + length), dStart))

let toMap (lines: string list) =
    let toRangeMap (line: string) =
        let ((sStart, sEnd), dStart) = line |> toMapSpec
        (fun n ->
            if n < sStart || sEnd <= n then None else
                Some <| (dStart + (n - sStart), 42))
    let ranges = lines |> List.tail |> List.map toRangeMap
    fun n ->
        ranges |> List.tryPick (fun range -> range n)
        |> Option.defaultValue (n, 42)

let multiMap maps n = (n, maps) ||> List.fold (fun n map -> map n |> fst)

let fillMapSpec (mapSpec: ((uint * uint) * uint) list) =
    let rec fill start specs filled =
        match specs with
        | [] -> (((start, UInt32.MaxValue), start) :: filled) |> List.rev
        | ((srcStart, srcEnd), _) as spec::rest when start = srcStart  ->
            fill srcEnd rest (spec :: filled)
        | ((srcStart, srcEnd), _) as spec::rest when start < srcStart  ->
            let idRange = ((start, srcStart), start)
            fill srcEnd rest (spec :: idRange :: filled)
        | ((srcStart, _), _) as spec::rest when start > srcStart  ->
            failwith "oops"
    fill 0u (mapSpec |> List.sortBy (fst >> fst)) []

let applyMap (spnStart, spnEnd) mapSpec =
    mapSpec
    |> List.skipWhile (fun ((_, srcEnd), _) -> srcEnd < spnStart)
    |> List.takeWhile (fun ((srcStart, _), _) -> srcStart < spnEnd  )
    |> List.map (fun ((srcStart, srcEnd), dstStart) ->
        let shift = dstStart - srcStart
        (shift + max spnStart srcStart, shift + min spnEnd srcEnd))


let part1 (getLines: string -> string list) =
    let paras = getLines "input" |> String.concat "\n" |> paragraphs
    let nums = paras.Head |> numbers
    let bigMap =
        paras |> List.tail
        |> List.map (fun p -> p.Split("\n") |> List.ofArray)
        |> List.map toMap
        |> multiMap
    nums |> List.map bigMap |> List.min

let part2 (getLines: string -> string list) =
    let paras = getLines "input" |> String.concat "\n" |> paragraphs
    let spans = paras.Head |> ranges
                |> List.map (fun (start, len) -> (start, start + len))
    let mapSpecs =
        paras
        |> List.tail
        |> List.map (fun p -> p.Split("\n") |> List.ofArray |> List.tail)
        |> List.map (List.map toMapSpec )
        |> List.map fillMapSpec
    (spans, mapSpecs) ||> List.fold (fun spans spec ->
        spans |> List.collect (fun span -> applyMap span spec))
    |> List.sortBy fst
    |> List.head
    |> fst
