module Day17

type City = int [] []
type Dir = N | E | S | W
type Position = {
        X : int
        Y : int
        Dir : Dir
    }

type Crucible = {
        Position: Position
        ConsecutiveCount: int
        Loss: int
        History: Position list
    }

type Records = Map<(Position * int), Crucible>

let parseLines (lines: string list) =
    let parseLine (line: string) =
        line.ToCharArray() |> Array.map (fun c -> c - '0' |> int)
    lines |> List.map parseLine |> Array.ofList

let initialCrucibles = [{
    Position = { X = 0; Y = 0; Dir = E }
    ConsecutiveCount = 0
    Loss = 0
    History = []
}]

let allDirs = [N; E; S; W]
let reverse = function N -> S | S -> N | E -> W | W -> E

let hyperthecalPostions (pstn: Position) =
    let (x, y) = pstn.X, pstn.Y
    let revDir = reverse pstn.Dir
    allDirs
    |> List.filter ((<>) revDir)
    |> List.map (function
        | N -> {X = x; Y = y - 1; Dir = N}
        | E -> {X = x + 1; Y = y; Dir = E}
        | S -> {X = x; Y = y + 1; Dir = S}
        | W -> {X = x - 1; Y = y; Dir = W})

let nextCruciblesSingle (city: City) minCons maxCons (crucible: Crucible) =
    hyperthecalPostions crucible.Position
    |> List.filter(fun pstn ->
        pstn.X >= 0 && pstn.X < city[0].Length
        && pstn.Y >= 0 && pstn.Y < city.Length)
    |> List.map (fun pstn ->
        {
            Position = pstn
            ConsecutiveCount =
                if pstn.Dir = crucible.Position.Dir then
                    crucible.ConsecutiveCount + 1
                else
                    1
            Loss = crucible.Loss + city[pstn.Y][pstn.X]
            History = pstn::crucible.History
        } )
    |> List.filter(fun crs ->
            (crs.Position.Dir = crucible.Position.Dir
             || crucible.ConsecutiveCount >= minCons
             // Covers initial crucible which can go in any direction
             // even when its conCount < minCon
             || crucible.ConsecutiveCount = 0)
            &&
            crs.ConsecutiveCount <= maxCons )

let nextCrucibles (city: City) minCons maxCons (crucibles: Crucible list) =
    crucibles |> List.collect (nextCruciblesSingle city minCons maxCons)

let recordBest (records: Records) (crucible: Crucible) =
    let key = (crucible.Position, crucible.ConsecutiveCount)
    match Map.tryFind key records with
    | None ->
        true, Map.add key crucible records
    | Some prevCruc when prevCruc.Loss > crucible.Loss ->
        true, Map.add key crucible records
    | _ -> false, records

let nextBestCrucibles city minStraight maxStraight (records, crucibles) =
    let candidates = nextCrucibles city minStraight maxStraight crucibles
    ((records, []), candidates)
    ||> List.fold (fun (records, keepers) candidate ->
        let (isKeeper, records') = recordBest records candidate
        let keepers' = if isKeeper then candidate::keepers else keepers
        (records', keepers'))

let rec alwaysBeBettering records city minCon maxCon crucs =
    let records, crucs = nextBestCrucibles city minCon maxCon (records, crucs)
    match crucs with
    | [] -> records
    | _ -> alwaysBeBettering records city minCon maxCon crucs

let bestFinish (city: City) minStraight (records: Records) =
    Map.values records
    |> Seq.filter (fun cruc ->
        cruc.Position.X = city[0].Length - 1
            && cruc.Position.Y = city.Length - 1
            && cruc.ConsecutiveCount >= minStraight)
    |> Seq.map _.Loss
    |> Seq.min

let bestRoute minCon maxCon city =
    alwaysBeBettering Map.empty city minCon maxCon initialCrucibles
    |> bestFinish city minCon

let part1 (getLines: string -> string list) =
    "input" |> getLines |> parseLines
    |> bestRoute 1 3

let part2 (getLines: string -> string list) =
    "input" |> getLines |> parseLines
    |> bestRoute 4 10
