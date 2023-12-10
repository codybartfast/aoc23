module Day10

type Direction = N | E | S | W

let parseLines lines =
    let parseLine (line: string) = line.ToCharArray()
    lines |> List.map parseLine |> Array.ofList

let partToDirections = function
    | '|' -> [N; S]
    | '-' -> [E; W]
    | 'L' -> [N; E]
    | 'J' -> [N; W]
    | '7' -> [S; W]
    | 'F' -> [S; E]

let directionsToPart dirs =
    match dirs |> List.sort with
    | [N; S] -> '|'
    | [E; W] -> '-'
    | [E; N] -> 'L'
    | [N; W] -> 'J'
    | [S; W] -> '7'
    | [E; S] -> 'F'

let complement = function N -> S | S -> N | W -> E | E -> W

let move (x, y) = function
    | N -> (x, y - 1)
    | E -> (x + 1, y )
    | S -> (x, y + 1)
    | W -> (x - 1, y)

let findConnected lookup start =
    [N; E; S; W]
    |> List.map (fun dir -> (dir, move start dir))
    |> List.filter (fun (dir, (x, y)) ->
        match lookup (x, y) with
        | '.' -> false
        | 'S' -> true
        | p -> partToDirections p |> List.contains (complement dir))

let next lookup (entryDir, (x, y)) =
    let part = lookup (x, y)
    let exitDir =
        partToDirections part
        |> List.filter ((<>) (complement entryDir))
        |> List.exactlyOne
    (exitDir, exitDir |> move (x, y))

let rec navigateLoop lookup start = seq{
        yield start
        let (_, nextCoord) as next = next lookup start
        if(lookup nextCoord <> 'S') then
            yield! navigateLoop lookup next }

let insideCountOfRow (row: char[]) =
    // flip 'inside' when we encounter "|", "F---J" or "L---7"
    //      but not for "F---7" or "L---J"
    ((0, false, ' '), row) ||> Array.fold (fun (count, inside, wallStart) tile ->
        match inside, wallStart, tile with
        | true, _, '.' -> count + 1, inside, wallStart
        | _,  _ , 'L' -> count, inside, 'L'
        | _,  _ , 'F' -> count, inside, 'F'
        | _,  _ , '|'
        | _, 'F', 'J'
        | _, 'L', '7' -> count, not inside, ' '
        | _ -> count, inside, wallStart)
    |> (fun (count, _, _) -> count)

let part1 getLines =
    let pipes : char[][] = "input" |> getLines |> parseLines
    let lookup (x, y) = pipes[y][x]

    let allCoords =
        [for y in 0 .. (pipes.Length - 1) do
            for x in 0 .. (pipes[0].Length - 1) do
                x,y]

    let start = allCoords |> List.find (lookup >> ((=) 'S'))
    let first = findConnected lookup start |> List.head

    navigateLoop lookup first
    |> Seq.length
    |> (fun n -> n / 2 + 1)

let part2 (getLines: string -> string list) =
    let pipes : char[][] = "input" |> getLines |> parseLines
    let lookup (x, y) = pipes[y][x]
    let set (x, y) c = pipes[y][x] <- c

    let allCoords =
        [for y in 0 .. (pipes.Length - 1) do
            for x in 0 .. (pipes[0].Length - 1) do
                x,y]

    let start = allCoords |> List.find (lookup >> ((=) 'S'))
    let [first; last] = findConnected lookup start

    let loopCoords = navigateLoop lookup first |> Seq.map snd |> Set

    // Replace pipe parts that aren't in the loop with '.'
    allCoords |> List.iter(fun coord ->
            if not <| loopCoords.Contains coord then set coord '.')

    // Put regular pipe part where 'S' was
    let startPart = [first; last] |> List.map fst |> directionsToPart
    set start startPart

    pipes |> Array.sumBy insideCountOfRow
