module Day10

let parseLines lines =
    let parseLine (line: string) = line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList

let partToDirections = function
    | '|' -> ["N"; "S"]
    | '-' -> ["E"; "W"]
    | 'L' -> ["N"; "E"]
    | 'J' -> ["N"; "W"]
    | '7' -> ["S"; "W"]
    | 'F' -> ["S"; "E"]
    | c ->  failwith $"Ooops what to do with {c}"

let directionsToPart dirs =
    match dirs |> List.sort with
    | ["N"; "S"] -> '|'
    | ["E"; "W"] -> '-'
    | ["E"; "N"] -> 'L'
    | ["N"; "W"] -> 'J'
    | ["S"; "W"] -> '7'
    | ["E"; "S"] -> 'F'

let complement = function "N" -> "S" | "S" -> "N" | "W" -> "E" | "E" -> "W"

let findConnected lookup (x, y) =
    [
        ("N", (x, y - 1));
        ("E", (x + 1, y))
        ("S", (x, y + 1));
        ("W",  (x - 1, y))
    ]
    |> List.filter (fun (dir, (x, y)) ->
        match lookup (x, y) with
        | '.' -> false
        | 'S' -> true
        | p -> partToDirections p |> List.contains (complement dir))

let move (x, y) = function
    | "N" -> (x, y - 1)
    | "E" -> (x + 1, y )
    | "S" -> (x, y + 1)
    | "W" -> (x - 1, y)

let next lookup (enterDir, (x, y)) =
    let part = lookup (x, y)
    let exitDir =
        partToDirections part
        |> List.filter ((<>) (complement enterDir))
        |> List.exactlyOne
    (exitDir, exitDir |> move (x, y))

let rec navigateLoop lookup (start: string * (int * int)) count = seq{
        yield (start, count)
        let (_, nCoord) as next = next lookup start
        if(lookup nCoord <> 'S') then
            yield! navigateLoop lookup next (count + 1)}

let rowInsideCount (row: char[]) =
    ((0, false, ' '), row) ||> Array.fold (fun (count, inside, wallStart) c ->
        match c, inside, wallStart with
        | '.', true, _ -> count + 1, inside, wallStart
        | '|', _, _
        | 'J', _, 'F'
        | '7', _, 'L' -> count, not inside, wallStart
        | 'L', _, _ -> count, inside, 'L'
        | 'F', _, _ -> count, inside, 'F'
        | _ -> (count, inside, wallStart))
    |> (fun (count, _, _) -> count)

let part1 getLines =
    let pipes = "input" |> getLines |> parseLines
    let lookup (x, y) = pipes[y][x]
    let coords =
        [for y in 0 .. (pipes.Length - 1) do
            for x in 0 .. (pipes[0].Length - 1) do
                x,y]

    let start =  coords |> List.find (lookup >> ((=) 'S'))
    let first = findConnected lookup start |> List.head
    navigateLoop lookup first 1
        |> List.ofSeq
        |> List.last |> snd
        |> (fun n -> n / 2 + 1)

let part2 (getLines: string -> string list) =
    let pipes = "input" |> getLines |> parseLines
    let lookup (x, y) = pipes[y][x]
    let set (x, y) c = pipes[y][x] <- c
    let coords =
        [for y in 0 .. (pipes.Length - 1) do
            for x in 0 .. (pipes[0].Length - 1) do
                x,y]

    let start =  coords |> List.find (lookup >> ((=) 'S'))
    let [first; last] = findConnected lookup start

    let pipeCoords =
        navigateLoop lookup first 1
        |> List.ofSeq
        |> List.map (fst >> snd)
        |> Set

    // Replace pipe parts that aren't in the loop wiht '.'
    coords
        |> List.iter(fun coord ->
            if not <| pipeCoords.Contains coord then set coord '.')

    // Put regular pipe part where 'S' was
    let startPart =  [first; last] |> List.map fst |> directionsToPart
    set start startPart

    pipes |> Array.sumBy rowInsideCount
