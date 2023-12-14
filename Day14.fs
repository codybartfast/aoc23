module Day14

open System
open System.Text.RegularExpressions

type Platform = char [] []


let parseLines lines =
    let parseLine (line: string) =
        line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList


let display (platform: Platform) =
    platform
    |> Array.iter (String >> printfn "%s")

let tiltRowWest (row: char []) =
    let rec tilt packed space todo =
        match todo with
        | [] -> (packed |> List.rev) @ space
        | '.'::rst -> tilt packed ('.'::space) rst
        | 'O'::rst -> tilt ('O'::packed) space rst
        | '#'::rst -> tilt ('#'::(space @ packed)) [] rst
    tilt [] [] (row |> List.ofArray) |> Array.ofList

let tiltWest (platform: Platform) =
    platform |> Array.map tiltRowWest

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

    |> tiltWest |> rotateRight
    |> tiltWest |> rotateRight
    |> tiltWest |> rotateRight
    |> tiltWest |> rotateRight

    |> rotateRight

let part1 getLines =
    "input" |> getLines  |> parseLines
    |> rotateLeft
    |> tiltWest
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
    | Some (n, p) when p = platform ->
        printfn $"{p = platform}"
        ((n, p), (nCycles, platform))
    | _ ->
        known <- known |> Map.add load (nCycles, platform)
        findDuplicate (nCycles + 1) (cycle platform)

let tooBig = 1000000000

let part2 getLines =
    let ((first, platform), (second, _)) = "input" |> getLines  |> parseLines |> findDuplicate 0
    let remaining = (tooBig - second) % (second - first)
    repeatCycle remaining platform
    |> totalLoad
