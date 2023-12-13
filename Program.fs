open System
open System.Diagnostics
open System.IO

open Day12
let day = "12"

let inline display partNo (result, (time: TimeSpan)) =
    let timePart = time.TotalSeconds.ToString("000.000000")
    printfn $"[{timePart}]  Part {partNo}: {result}"

let time action =
    let sw = Stopwatch()
    sw.Start()
    let rslt = action ()
    sw.Stop()
    (rslt, sw.Elapsed)

let getLines day file =
    Path.Combine("../../../input/2023", $"day{day}", $"{file}.txt")
    |> File.ReadAllLines
    |> List.ofArray

[<EntryPoint>]
let main _ =
    if true then
        printfn ""
        printfn $"Warming up ... "
        (fun () -> part1 (getLines day)) |> time |> ignore
        (fun () -> part2 (getLines day)) |> time |> ignore

    printfn ""
    (fun () -> part1 (getLines day)) |> time |> (display 1)
    (fun () -> part2 (getLines day)) |> time |> (display 2)
    0
