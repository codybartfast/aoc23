module Day03

open System.Text.RegularExpressions

type Number =
    { Value: int; TopLeft: int*int; BottomRight: int*int}

let parse lines =
    let rxNum = Regex(@"\d+")
    let rxSym = Regex(@"[^.\d]")
    let parseSyms y line =
        rxSym.Matches(line)
        |> Seq.map(fun m -> ((m.Index, y), m.Value))
        |> List.ofSeq
    let parseNums y line =
        rxNum.Matches(line)
        |> Seq.map(fun mtch ->
            let x = mtch.Index
            {   Value = int mtch.Value
                TopLeft = (x - 1, y - 1)
                BottomRight = (x + mtch.Value.Length, y + 1) })
        |> List.ofSeq

    let syms = lines |> List.mapi parseSyms |> List.collect id
    let nums = lines |> List.mapi parseNums |> List.collect id
    (syms, nums)

let touches number symbol =
    let (x, y), _ = symbol
    let {TopLeft = (lx, ty); BottomRight = (rx, by)} = number
    (lx <= x && x <= rx) && (ty <= y && y <= by)

let part1 getLines =
    let (symbols, numbers) =  getLines "input" |> parse
    numbers
        |> List.filter (fun num -> symbols |> List.exists (touches num))
        |> List.sumBy(fun num -> num.Value)

let part2 getLines  =
    let (symbols, numbers) =  getLines "input" |> parse
    symbols
        |> List.filter (snd >> ((=) "*"))
        |> List.map (fun star ->
            numbers |> List.filter(fun num -> touches num star ))
        |> List.filter (List.length >> ((=) 2))
        |> List.sumBy (List.map (fun num -> num.Value) >> List.reduce  (*))
