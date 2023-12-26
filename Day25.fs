module Day25

type Connections = Map<string, string list>
type Wire = (string * string)
type Path = string list

let parseLines (lines: string list) =
    let parseLine (line: string) =
        let [| left; rightTxt |] = line.Split(':')
        rightTxt.Trim().Split(' ')
        |> List.ofArray
        |> List.map (fun right -> (left, right))
    lines |> List.collect parseLine |> List.distinct

let makeConnections wires =
    let addConn (a, b) connections =
        let newRemotes =
            match Map.tryFind a connections with
            | None -> [b]
            | Some lst -> b::lst
        Map.add a newRemotes connections
    let addWire connections (a, b) =
        connections |> addConn (a, b) |> addConn (b, a)
    (Map.empty, wires)
    ||> List.fold addWire

let removeConn conns (a, b)  =
    conns
    |> Map.add a (conns[a] |> List.filter ((<>) b))
    |> Map.add b (conns[b] |> List.filter ((<>) a))

let extendPaths (conns: Connections) (visited, paths) =
    let extend (visited: Set<string>) (path: Path) =
        let part = path.Head
        let nexts = conns[part] |> List.filter (visited.Contains >> not)
        let newPaths = nexts |> List.map (fun nxt -> nxt::path)
        newPaths, (Set.add part visited)
    let newPathsList, visited = (visited, paths) ||> List.mapFold extend
    (visited, newPathsList |> List.collect id)

let findPath conns (a, b)  =
    let rec findPath (visited, paths) =
        if paths = [] then None else
            match paths |> List.filter (List.head >> ((=) b)) with
            | [] -> extendPaths conns (visited, paths) |> findPath
            | path::_ -> Some path
    findPath (Set.singleton a, [[a]])

let rec trySplit maxRems conns path (removed: Wire list)  =
    if removed.Length = maxRems then None else
        path
        |> List.pairwise
        |> List.tryPick(fun wire->
            let conns = removeConn conns wire
            let removed = wire::removed
            match findPath conns wire with
            | None -> Some (removed.Head, conns)
            | Some altPath -> trySplit maxRems conns altPath removed)

let size conns start =
    let rec size (visited, paths) =
        match paths with
        | [] -> Set.count visited
        | _ -> extendPaths conns (visited, paths) |> size
    size (Set.empty, [[start]])

let part1 (getLines: string -> string list) =
    let wires = "input" |> getLines  |> parseLines
    let conns = wires |> makeConnections
    let (start1, start2), conns =
        wires
        |> List.tryPick (fun (a, b) -> trySplit 3 conns [a; b] [])
        |> Option.get
    (size conns start1) * (size conns start2)

let part2 (getLines: string -> string list) =
    "Merry Christmas, and a peaceful new year"
