module Day14

open System
open System.Text.RegularExpressions

type Platform = char [] []

let parseLines lines =
    let parseLine (line: string) =
        line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList

let tiltLeft (platform: Platform) =
    let rec findDest (row: char[]) i =
        let nxt = i - 1
        if nxt >= 0 && row[nxt] = '.' then findDest row nxt else  i
    let tiltRowWest (row: char []) =
        row |> Array.iteri (fun i c ->
            if c = 'O' then
                let dest = findDest row i
                row[i] <- '.'
                row[dest] <- 'O' )
    platform |> Array.iter tiltRowWest
    platform

let rotateRight (platform: Platform) =
    platform |> Array.transpose |> Array.map Array.rev

let rotateLeft (platform: Platform) =
    platform |> Array.map Array.rev |> Array.transpose

let totalLoad (platform: Platform) =
    platform
    |> rotateLeft
    |> rotateLeft
    |> Array.mapi (fun i row -> (i + 1) * (row |> Array.filter ((=) 'O') |> Array.length))
    |> Array.sum

let cycle (platform: Platform) =
    platform
    |> rotateLeft

    |> tiltLeft |> rotateRight
    |> tiltLeft |> rotateRight
    |> tiltLeft |> rotateRight
    |> tiltLeft |> rotateRight

    |> rotateRight

let part1 getLines =
    "input" |> getLines  |> parseLines
    |> rotateLeft
    |> tiltLeft
    |> rotateRight
    |> totalLoad

let rec repeatCycle n (platform:Platform) =
    if n = 0 then platform else
        if n % 100000 = 0 then printfn $"{n / 100000}"
        repeatCycle (n - 1) (cycle platform)

let mutable known = Map.empty<int, int * Platform>

let rec findDuplicate (nCycles: int) (platform: Platform) =
    let load = totalLoad platform
    match Map.tryFind load known with
    | Some (n, p) when p = platform -> ((n, p), (nCycles, platform))
    | _ ->
        known <- known |> Map.add load (nCycles, platform)
        findDuplicate (nCycles + 1) (cycle platform)

let tooBig = 1000000000

let part2 getLines =
    known <- Map.empty
    let ((first, platform), (second, _)) = "input" |> getLines  |> parseLines |> findDuplicate 0
    let remaining = (tooBig - second) % (second - first)
    repeatCycle remaining platform
    |> totalLoad
