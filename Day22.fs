module Day22

type Brick = { Id: string; X: int; X': int; Y: int; Y': int; Z: int; Z': int }
type Height = { mutable Height: int; mutable Brick: string }
type Heights = Height [,]

let parseLines (lines: string list) =
    let parseLine i (line: string) =
        let [| aTxt; bTxt |] = line.Split('~')
        let coords (txt: string) = txt.Split(',') |> Array.map int
        let [| x; y; z |] = coords aTxt
        let [| x'; y'; z' |] = coords bTxt
        assert (x <= x' && y <= y' && z <= z')
        {Id = (i+1).ToString(); X = x; X' = x'; Y = y; Y' = y'; Z = z; Z' = z'}

    lines |> List.mapi parseLine

let initHeights bricks : Heights =
    let xMax = bricks |> List.map _.X' |> List.max
    let yMax = bricks |> List.map _.Y' |> List.max
    Array2D.init (xMax + 1) (yMax + 1) (fun _ _ -> {Height = 0; Brick = "F" })

let addBrick (heights: Heights) brick =
    let shadow =
        heights[brick.X .. brick.X', brick.Y .. brick.Y']
        |> Seq.cast<Height>
        |> List.ofSeq

    let supportHeight = shadow |> List.map _.Height |> List.max

    let supprtingBricks =
        shadow
        |> List.filter (_.Height >> ((=) supportHeight))
        |> List.map _.Brick
        |> Set

    shadow
    |> List.iter (fun h ->
        h.Height <- (supportHeight + brick.Z' - brick.Z + 1)
        h.Brick <- brick.Id)

    (brick.Id, supprtingBricks)

let findVital =
        List.filter (snd >> Set.count >> ((=) 1))
        >> List.collect (snd >> Set.toList >> id)
        >> List.filter ((<>) "F")
        >> List.distinct

let rec allDependants relations brick =
    let directDependants bricks =
        List.filter (snd >> (fun sups ->  Set.isSubset sups bricks))
        >> List.map fst
        >> Set
    let rec repeat dependants  =
        let nextGen = Set.difference (directDependants dependants relations ) dependants
        match nextGen |> Set.isEmpty with
        | true -> dependants
        | false -> repeat (Set.union dependants nextGen)
    repeat (Set.singleton brick)

let part1 (getLines: string -> string list) =
    let bricks = "input" |> getLines  |> parseLines |> List.sortBy _.Z
    let relations = bricks |> List.map (addBrick (initHeights bricks))
    bricks.Length - (findVital relations).Length

let part2 (getLines: string -> string list) =
    let bricks = "input" |> getLines  |> parseLines |> List.sortBy _.Z
    let relations = bricks |> List.map (addBrick (initHeights bricks))
    findVital relations
    |> List.sumBy ((allDependants relations) >> Set.count >> ((+) -1))
