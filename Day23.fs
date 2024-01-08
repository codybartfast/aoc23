module Day23

type Coord = (int * int)
type Park = char [] []
type Direction = N | E | S | W
type Link = {A: Coord; B: Coord; Len: int; Ice: bool}
type Links = Map<Coord, Link list>
type Path = { Head: Coord; Len: int; Visited: Set<Coord> }

let parseLines (lines: string list) =
    lines |> List.map _.ToCharArray() |> Array.ofList

let adjacent (park: Park) (x, y) =
    [(N, (x, y - 1)); (E, (x + 1, y)); (S, (x, y + 1)); (W, (x - 1, y))]
    |> List.filter (fun (_, (x, y)) ->
        0 <= x && x < park[0].Length
            && 0 <= y && y < park.Length)
    |> List.filter(fun (dir, (x, y)) ->
        let c = park[y][x]
        c = '.'
        || dir = N && c = '^'
        || dir = E && c = '>'
        || dir = S && c = 'v'
        || dir = W && c = '<')
    |> List.map snd

let unvisited (park: Park) (coord: Coord) (visited: Set<Coord>) =
    adjacent park coord |> List.filter ((visited.Contains) >> not)

let meltIce (park: Park) =
    [for y in 0 .. park.Length - 1 do
         for x in 0 .. park[0].Length - 1 do
             let c = park[y][x]
             if c = '^' || c = '>' || c = 'v' || c = '<' then
                 park[y][x] <- '.' ] |> ignore
    park

let rec walkLink (park: Park) start len prev ice visited ((x, y) as coord) =
    let visited = Set.add coord visited
    let ice = ice || park[y][x] <> '.'
    let nextCoords = adjacent park coord |> List.filter ((<>) prev)
    match nextCoords.Length with
    | 1 -> walkLink park start (len + 1) coord ice visited nextCoords.Head
    | _ -> { A = start; B = coord; Len = len; Ice = ice }, visited

let nextLinks (park: Park) (visited: Set<Coord>) (tip: Coord) =
    let nextCoords = unvisited park tip visited
    (visited, nextCoords) ||> List.mapFold (walkLink park tip 1 tip false)

let rec findLinks (park: Park) visited tips links =
    if tips = [] then links else
        let (linkLists, visited) =
            (visited, tips) ||> List.mapFold (nextLinks park)
        let newLinks = linkLists |> List.collect id
        let tips = newLinks |> List.map _.B
        findLinks park visited tips (newLinks @ links)

let linksMap (park: Park) =
    let linksOne = findLinks park (Set.singleton (1, 0)) [(1,0)] []
    let iceFree = linksOne |> List.filter (_.Ice >> not)
    let linksRev = iceFree |> List.map (fun link ->
        {A = link.B; B = link.A; Len = link.Len; Ice = link.Ice})
    linksOne @ linksRev |> List.groupBy _.A |> Map

let extendpath (links: Links) (path: Path) =
    let _, y = path.Head
    if y > 0 && y % 2 = 0 then [] else
        links[path.Head]
        |> List.filter (_.B >> path.Visited.Contains >> not)
        |> List.map (fun lnk ->
            let visited = Set.add lnk.B path.Visited
            {Head = lnk.B; Len = path.Len + lnk.Len; Visited = visited })

let rec findLongest (links: Links) rslt (paths: Path list)  =
    let throughRoutes =
        paths |> List.filter (_.Head >> snd >> (fun y -> y % 2 = 0))
    let rslt =
        if throughRoutes = [] then rslt else
            throughRoutes |> List.map (_.Len) |> List.max |> max rslt
    if paths = [] then rslt else
        paths |> List.collect (extendpath links) |> findLongest links rslt

let initialPath = {Head = (1, 0); Len = 0; Visited = Set.singleton (1, 0)}

let part1 (getLines: string -> string list) =
    let links =
        "input" |> getLines  |> parseLines
        |> linksMap
    findLongest links 0 [initialPath]

let part2 (getLines: string -> string list) =
    let links =
        "input" |> getLines  |> parseLines
        |> meltIce
        |> linksMap
    findLongest links 0 [initialPath]
