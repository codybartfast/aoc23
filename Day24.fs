module Day24

open System

let pi = Math.PI

type Stone = { X: float; Y: float; Z: float; VX: float; VY: float; VZ: float }

let parseLines (lines: string list) =
    let parseLine (line: string) =
        let [|x; y; z; dx; dy; dz|] =
            line.Split(" ,@".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            |> Array.map float
        {X = x; Y = y; Z = z; VX = dx; VY = dy; VZ = dz}
    lines |> List.map parseLine

let rec hcf a b = if b = 0.0 then a else hcf b (a % b)
let rec lcm x y =
    x * (y / (hcf x y))

let reduce (a, b) =
    let hcf = hcf a b
    let rslt = (a / hcf, b / hcf)
    assert ((snd rslt) >= 0.0)
    rslt

let relGrad (a: Stone) (b: Stone) =
    let xLcm = lcm a.VX b.VX
    let aFact = xLcm / a.VX
    let bFact = xLcm / b.VX
    let a' = (a.VX * aFact, a.VY * aFact)
    let b' = (b.VX * bFact, b.VY * bFact)
    (snd b' - snd a' |> float) / (fst b' |> float)

let alignB (a: Stone) (b: Stone)  =
    let bGrad = (b.VY |> float) / (b.VX |> float)
    let a = (a.X |> float), (a.Y |> float)
    let b = (b.X |> float), (b.Y |> float)
    a, ((fst a), (snd b) - (((fst b) - (fst a)) * bGrad))

let findXYIntersect (a: Stone) (b: Stone) =
    let relGrad = relGrad a b
    let aCoord, bCoord = alignB a b
    let vertDist = snd bCoord - snd aCoord
    let xDiff = vertDist / relGrad
    let intX = (fst aCoord) - xDiff
    let intY = (snd aCoord) - xDiff * (a.VY |> float) / (a.VX |> float)
    let aTime = (intX - (a.X |> float)) / (a.VX |> float)
    let bTime = (intX - (b.X |> float)) / (b.VX |> float)
    (intX, intY), (aTime, bTime)

let allIntersections (stones: Stone list) =
    let rec allIntersections stonesA stonesB intersections =
        match stonesA, stonesB with
        | [_], _
        | [], _ -> intersections
        | _::((_::(restB)) as restA), [] ->
            allIntersections restA restB intersections
        | (a::_), (b::restB) ->
            allIntersections stonesA restB (findXYIntersect a b :: intersections)
    allIntersections stones stones.Tail []

let inScope low high =
    (fun ((x, y), (aTime, bTime)) ->
        low <= x && x <= high
        && low <= y && y <= high
        && 0.0 <= aTime && 0.0 <= bTime)

let part1 (getLines: string -> string list) =
    let stones = "input" |> getLines  |> parseLines
    let inScope = inScope 200000000000000.0 400000000000000.0
    allIntersections stones
    |> List.filter inScope
    |> List.length
    |> fun x -> String.Join(" | ", x)

let rotateY rot (stn: Stone) =
    let x = Math.Cos rot * stn.X - Math.Sin rot * stn.Z
    let z = Math.Cos rot * stn.Z + Math.Sin rot * stn.X
    let vx = Math.Cos rot * stn.VX - Math.Sin rot * stn.VZ
    let vz = Math.Cos rot * stn.VZ + Math.Sin rot * stn.VX
    {   X = x; Y = stn.Y; Z = z; VX = vx; VY = stn.VY; VZ = vz }

let rotateX rot (stn: Stone) =
    let z = Math.Cos rot * stn.Z - Math.Sin rot * stn.Y
    let y = Math.Cos rot * stn.Y + Math.Sin rot * stn.Z
    let vz = Math.Cos rot * stn.VZ - Math.Sin rot * stn.VY
    let vy = Math.Cos rot * stn.VY + Math.Sin rot * stn.VZ
    { X = stn.X; Y = y; Z = z; VX = stn.VX; VY = vy; VZ = vz }

let measure (stones: Stone list) =
    let rec measure intersections minX maxX minY maxY =
        match intersections with
        | [] -> (maxX - minX) + (maxY - minY), ((minX, minY), (maxX, maxY))
        | ((x, y), _)::tail ->
            measure tail (min minX x) (max maxX x) (min minY y) (max maxY y)
    measure (allIntersections stones)
        Double.MaxValue Double.MinValue Double.MaxValue Double.MinValue

let measureAngle stones (angX, angY) =
    stones |> List.map (rotateX angX >> rotateY angY) |> measure

let angles (x, y) range parts =
    let step = range / parts
    [for xDiff in 0.0 .. step .. range do
         for yDiff in 0.0 .. step .. range do
             [(x + xDiff, y + yDiff)
              (x + xDiff, y - yDiff)
              (x - xDiff, y + yDiff)
              (x - xDiff, y - yDiff)]]
    |> List.collect id

let rec improve parts stones (angX, angY) range prevMeasure =
    // printfn $"Trying rotations starting with xRot:{angX}, yRot:{angY}, +/- {range} rad, in {parts} steps ... "
    let (angX', angY'), (measure, coords) =
        angles (angX, angY) range parts
        |> List.map (fun (angX, angY) ->
            (angX, angY), measureAngle stones (angX, angY))
        |> List.filter (snd >> fst >> Double.IsNormal)
        |> List.minBy snd
    let improv = prevMeasure - measure
    // printfn $"    ... at xRot:{angX'}, yRot:{angY'} found convergence zone with width+height={measure} (improvement of {improv})."
    match improv with
    | 0.0 -> ((angX', angY'), coords)
    | _ -> improve parts stones (angX', angY') (2.0 * range / parts) measure

let intersectionTimes stones stone =
    stones
    |> List.filter ((<>) stone)
    |> List.map ((findXYIntersect stone) >> snd >> fst)
    |> List.map Double.Round
    |> List.distinct
    |> List.exactlyOne

let positionAtTime stone time =
    (stone.X + (stone.VX * time)),
        (stone.Y + (stone.VY * time)),
        (stone.Z + (stone.VZ * time))

let part2 (getLines: string -> string list) =
    let stones = "input" |> getLines  |> parseLines
    let (angX, angY), ((minX, minY), (maxX, maxY)) =
        improve 17 stones (0.0, 0.0) (pi / 2.0) Double.MaxValue

    let czRangeX = maxX - minX
    let czRangeY = maxY - minY
    let stoneData =
        stones
        |> List.map (fun stn ->
            let rotStn = stn |> (rotateX angX >> rotateY angY)
            let relX, relY = rotStn.VX / czRangeX, rotStn.VY / czRangeY
            (max (abs relX) (abs relY), (stn, rotStn)))
        |> List.sortByDescending fst
    let rotatedStones = stoneData |> List.map (snd >> snd)
    let [(_, (stn1, rStn1)); (_, (stn2, rStn2))] = stoneData |> List.take 2
    let time1 = intersectionTimes rotatedStones rStn1
    let time2 = intersectionTimes rotatedStones rStn2
    let (x1, y1, z1) = positionAtTime stn1 time1
    let (x2, y2, z2) = positionAtTime stn2 time2
    let time = time2 - time1
    let vx, vy, vz = ((x2 - x1) / time), ((y2 - y1) / time), ((z2 - z1) / time)
    let x, y, z = (x1 - (time1 * vx), y1 - (time1 * vy), z1 - (time1 * vz))
    x + y + z
