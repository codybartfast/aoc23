module Day02

let parseLine (line: string) =
    let parseHF (txt: string) =
        txt.Split(",")
        |> List.ofArray
        |> List.map (fun kind ->
            let [| countTxt; colour |] = kind.Trim().Split(" ")
            (colour, int countTxt))

    let [| head; tail |] = line.Split(":")
    let id = int <| head.Split(" ").[1]
    let handfuls =
        tail.Split(";")
        |> Array.map parseHF
        |> List.ofArray
    (id, handfuls)

let possible red green blue handFulls =
    let hfPossible hf =
        match hf with
        | "red", count -> count <= red
        | "green", count -> count <= green
        | "blue", count -> count <= blue

    handFulls |> List.forall (List.forall hfPossible)

let power handFulls =
    handFulls
    |> List.collect id
    |> List.fold
        (fun (r, g, b) (colour, count) ->
            match colour with
            | "red" -> (max r count, g, b)
            | "green" -> (r, max g count, b)
            | "blue" -> (r, g, max b count))
        (0, 0, 0)
    |> (fun (r, g, b) -> r * g * b)

let part1 getLines =
    getLines "input"
    |> List.map parseLine
    |> List.filter (snd >> (possible 12 13 14))
    |> List.sumBy fst

let part2 getLines =
    getLines "input"
    |> List.sumBy (parseLine >> snd >> power)
