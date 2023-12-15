module Day15

type Lens = string * int
type Box = Lens list
type Config = Box []
type Instruction =
    | Remove of string * int
    | Insert of string * int * int

let updateHash current char = current + int char |> ((*) 17) |> fun n -> n % 256
let hash (txt: string) = txt.ToCharArray() |> Array.fold updateHash 0

let instruction (txt: string) =
    if txt.EndsWith("-") then
        let label = txt[0 .. ^1]
        Remove (label, hash label)
    else
        let [| label; focalLength |] = txt.Split('=')
        Insert (label, hash label, int focalLength)

let remove box lbl =
    match box |> List.tryFindIndex (fun (lens: Lens) -> (fst lens) = lbl) with
    | None -> box
    | Some idx -> box |> List.removeAt idx

let insert box lbl fl =
    match box |> List.tryFindIndex (fun (lens: Lens) -> (fst lens) = lbl) with
    | None -> (lbl, fl)::box
    | Some idx -> box |> List.removeAt idx |> List.insertAt idx (lbl, fl)

let execute (config: Config) (instr: Instruction) =
    match instr with
    | Remove (lbl, boxId) -> config[boxId] <- remove config[boxId] lbl
    | Insert (lbl, boxId, fl) -> config[boxId] <- insert config[boxId] lbl fl
    config

let boxPower boxIdx =
    List.rev
    >> List.mapi(fun lensIdx (_, fl) -> (boxIdx + 1) * (lensIdx + 1) * fl)
    >> List.sum

let part1 (getLines: string -> string list) =
    "input" |> getLines |> List.head |> _.Split(',')
    |> Array.sumBy hash

let part2 (getLines: string -> string list) =
    "input" |> getLines |> List.head |> _.Split(',')
    |> Array.map instruction
    |> Array.fold execute (Array.create 256 [])
    |> Array.mapi boxPower
    |> Array.sum
