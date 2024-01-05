module Day21

type Farm = char [] []

type Counts = {
    CentreEven: int64; CentreOdd: int64
    North: int64; NE1 : int64; NE2 : int64
    East: int64; SE1 : int64; SE2 : int64
    South: int64; SW1 : int64; SW2 : int64
    West: int64; NW1 : int64; NW2 : int64
}

let rec repeat func n x = if n = 0 then x else repeat func (n - 1) (func x)

let parseLines (lines: string list) =
    let parseLine (line: string) =
        line.Replace("S", ".").ToCharArray()
    lines |> List.map parseLine |> Array.ofList

let midCoord (farm: Farm) =
    let mid n =
        if n % 2 = 1 then n / 2 else failwith $"Expected odd nubmer got {n}"
    mid farm[0].Length, mid farm.Length

let markMid (farm: Farm) =
    let (x, y) = midCoord farm
    farm[y][x] <- 'O'
    farm

let adjacents (farm: Farm) (x, y) =
    let (width, height) = farm[0].Length, farm.Length
    [(x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y)]
    |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)

let nextSteps (farm: Farm, lastSteps : (int * int) list) =
    let newSteps =
        lastSteps
        |> List.collect (adjacents farm)
        |> List.distinct
        |> List.filter (fun (x, y) -> farm[y][x] = '.')
    lastSteps
    |> List.iter (fun (x, y) -> farm[y][x] <- '.')
    newSteps
    |> List.iter (fun (x, y) -> farm[y][x] <- 'O')
    (farm, newSteps)

let count = Array.collect id >> Array.filter ((=) 'O') >> Array.length

let multiFarm (baseFarm: Farm) steps =
    let baseLen = baseFarm.Length
    assert (baseFarm[0].Length = 3)
    let maxWidth = 1 + 2 * steps
    let dups = (maxWidth / baseLen) + if maxWidth % baseLen = 0 then 0 else 1

    let dups = if dups % 2 = 0 then dups + 1 else dups
    let len = baseLen * dups

    let farm = Array.init len (fun _ -> Array.init len (fun _ -> '.'))
    for xm in 0 .. dups - 1 do
        for ym in 0 .. dups - 1 do
            for x in 0 .. baseLen - 1 do
                for y in 0 .. baseLen - 1 do
                    farm[ym * baseLen + y][xm * baseLen + x] <- baseFarm[y][x]
    farm

let multiCount (baseFarm: Farm) (farm: Farm) =
    let len = baseFarm.Length
    if farm.Length % len <> 0 then failwith "Bad Fit"
    let dups = farm.Length / len
    [for Y in 0 .. dups - 1 do
         [for X in 0 .. dups - 1 do
              [for y in 0 .. len - 1 do
                for x in 0 .. len - 1 do (x, y)]
              |> List.filter
                     (fun (x, y) -> farm[(Y * len) + y][(X * len) + x] = 'O')
              |> List.length
              |> int64 ]]

let makeCounts (baseFarm: Farm) =
    let baseLen = baseFarm.Length
    let steps = baseLen / 2 + (2 * baseLen)
    let farm = multiFarm baseFarm steps |> markMid
    let allCounts =
        (farm, [midCoord farm])
        |> repeat nextSteps steps
        |> fst |> multiCount baseFarm
    {
        CentreEven = allCounts[2][2]; CentreOdd =  allCounts[1][2]
        North = allCounts[0][2]; NE1 = allCounts[0][3]; NE2 = allCounts[1][3]
        East = allCounts[2][4]; SE1 = allCounts[3][4]; SE2 = allCounts[3][3]
        South = allCounts[4][2]; SW1 = allCounts[4][1]; SW2 = allCounts[3][1]
        West = allCounts[2][0]; NW1 = allCounts[1][0]; NW2 = allCounts[1][1]
    }

let calc (cs: Counts) n =
    let rows = 2 * n + 1
    let midY = rows / 2
    let centre = if n % 2 = 0 then cs.CentreEven else cs.CentreOdd
    let fill y = ((midY - (abs (midY - y))) |> int64)
                     * (cs.CentreEven + cs.CentreOdd) - centre
    let northTip = cs.NW1 + cs.North + cs.NE1
    let north =
        [1 .. midY - 1]
        |> List.sumBy (fun y -> cs.NW1 + cs.NW2 + fill(y) + cs.NE2 + cs.NE1)
    let centre = cs.West + fill(midY) + cs.East
    let south =
        [midY + 1 .. rows - 2]
        |> List.sumBy (fun y -> cs.SW1 + cs.SW2 + fill(y) + cs.SE2 + cs.SE1)
    let southTip = cs.SW1 + cs.South + cs.SE1
    northTip + north + centre + south + southTip

let part1 (getLines: string -> string list) =
    let farm = "input" |> getLines  |> parseLines |> markMid
    (farm, [midCoord farm])
    |> (repeat nextSteps 64)
    |> fst |> count

let part2 (getLines: string -> string list) =
    let steps = 26501365
    let baseFarm = "input" |> getLines  |> parseLines
    let counts = makeCounts baseFarm
    let baseLen = baseFarm.Length
    let n = (steps - (baseLen / 2)) / baseLen
    calc counts n

(*
    Base case, n = 0, is 65 steps to the edgoe of the base map.
    The 'n + 1' case is the 'n' case with an additional 131 steps.

    Steps when n = 0: 65 + (0 * 131)

                                   3762


    Steps when n = 1: 65 + (1 * 131)

                             921 | 5574 |  934
                            5585 | 7436 | 5601
                             939 | 5612 |  945



    Steps when n = 2: 65 + (2 * 131) - This contains all the numbers we need

                          |  921 | 5574 |  934 |
                      921 | 6497 | 7436 | 6501 |  934
                     5585 | 7436 | 7424 | 7436 | 5601
                      939 | 6512 | 7436 | 6524 |  945
                          |  939 | 5612 |  945 |



    Steps when n = 3: 65 + (3 * 131)

                   |      |  921 | 5574 |  934 |      |
                   |  921 | 6497 | 7436 | 6501 |  934 |
               921 | 6497 | 7436 | 7424 | 7436 | 6501 |  934
              5585 | 7436 | 7424 | 7436 | 7424 | 7436 | 5601
               939 | 6512 | 7436 | 7424 | 7436 | 6524 |  945
                   |  939 | 6512 | 7436 | 6524 |  945 |
                   |      |  939 | 5612 |  945 |      |



    Steps when n = 4: 65 + (4 * 131)

            |      |      |  921 | 5574 |  934 |      |      |
            |      |  921 | 6497 | 7436 | 6501 |  934 |      |
            |  921 | 6497 | 7436 | 7424 | 7436 | 6501 |  934 |
        921 | 6497 | 7436 | 7424 | 7436 | 7424 | 7436 | 6501 |  934
       5585 | 7436 | 7424 | 7436 | 7424 | 7436 | 7424 | 7436 | 5601
        939 | 6512 | 7436 | 7424 | 7436 | 7424 | 7436 | 6524 |  945
            |  939 | 6512 | 7436 | 7424 | 7436 | 6524 |  945 |
            |      |  939 | 6512 | 7436 | 6524 |  945 |      |
            |      |      |  939 | 5612 |  945 |      |      |

*)
