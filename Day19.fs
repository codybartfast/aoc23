module Day19

open System
open System.Text.RegularExpressions

let splitOn sep =
    let rec split part parts = function
        | [] -> ((part |> List.rev) :: parts) |> List.rev
        | h :: t when h  = sep -> split [] ((part |> List.rev) :: parts) t
        | h :: t -> split (h :: part) parts t
    split [] []

type Comp = LT | GT
type Prop = X | M | A | S
type Ranges = Map<Prop, int * int>

type SubRule = {
    Prop: Prop
    Comp: Comp
    CompValue: int
    Rule: string }

type Rules = Map<string, (SubRule list * string)>

let rangeMin = 1
let rangeMax = 4000
let fullFranges =
    [X; M; A; S] |> List.map (fun p -> (p, (rangeMin, rangeMax))) |> Map

let parseLines (lines: string list) =
    let toSubRule propTxt compTxt compValueTxt rule =
        let prop = match propTxt with "x" -> X | "m" -> M | "a" -> A | "s" -> S
        let comp = match compTxt with "<" -> LT | ">" -> GT
        let compValue = Int32.Parse compValueTxt
        {Prop = prop; Comp = comp; CompValue = compValue; Rule = rule}
    let parseRule (line: string) =
        let mtch = Regex.Match(
            line, @"(?<name> \w+)\{(?<subRules>(.*,)+) (?<fallback> \w+)\}",
            RegexOptions.IgnorePatternWhitespace)
        let name = mtch.Groups["name"].Value
        let alternate = mtch.Groups["fallback"].Value
        let subRules =
            mtch.Groups["subRules"].Value.Trim(',').Split(',')
            |> Array.map(fun cond ->
                let subMtch = Regex.Match(cond, @"(\w)([<>])(\d+):(\w+)")
                toSubRule
                    subMtch.Groups[1].Value
                    subMtch.Groups[2].Value
                    subMtch.Groups[3].Value
                    subMtch.Groups[4].Value )
            |> List.ofArray
        name, (subRules, alternate)
    let parsePart (line: string) =
        let [| x; m; a; s |] =
            line[1..^1].Split(',')
            |> Array.map (_.Split('=') >> Array.item 1 >> Int32.Parse)
        Map [(X, (x, x)); (M, (m, m)); (A, (a, a)); (S, (s, s))]
    let [rules ; parts] = lines |> splitOn ""
    rules |> List.map parseRule |> Map, parts |> List.map parsePart

let splitRange (ranges: Ranges) (subRule: SubRule) =
    let propMin, propMax = ranges[subRule.Prop]
    let matchingMinMax, complementMinMax =
        match subRule.Comp with
        | LT -> (propMin, min propMax (subRule.CompValue - 1)),
                    (max propMin subRule.CompValue, propMax)
        | GT -> (max propMin (subRule.CompValue + 1), propMax),
                    (propMin, min propMax subRule.CompValue)
    ranges.Add (subRule.Prop, matchingMinMax),
        ranges.Add (subRule.Prop, complementMinMax)

let isValidRange (ranges: Ranges) =
    ranges
    |> Map.toList
    |> List.forall (fun (_, (propMin, propMax)) ->
        propMin <= propMax
        && propMin >= rangeMin
        && propMax <= rangeMax)

let rec applyRules (rules: Rules) (rule: string, ranges: Ranges) : Ranges list =
    if rule = "R" || not <| isValidRange ranges then
        []
    elif rule = "A" then
        [ranges]
    else
        let subRules, fallback = rules[rule]
        let subResults, unmatched =
            (ranges, subRules)
            ||> List.mapFold (fun range subRule ->
                let matchRange, complRange = splitRange range subRule
                (subRule.Rule, matchRange), complRange)
        (subResults |> List.collect (applyRules rules))
            @ applyRules rules (fallback, unmatched)

let rangeSum =  Map.toList >> List.map snd >> List.sumBy fst

let rangeSize =
    Map.toList
    >> List.map snd
    >> List.map (fun (mn, mx) -> 1L + (int64 mx) - (int64 mn))
    >> List.reduce (*)

let part1 (getLines: string -> string list) =
    let (rules, parts) = "input" |> getLines  |> parseLines
    parts
    |> List.map (fun part -> applyRules rules ("in", part))
    |> List.filter ((<>) [])
    |> List.map List.exactlyOne
    |> List.sumBy rangeSum

let part2 (getLines: string -> string list) =
    let (rules, _parts) = "input" |> getLines  |> parseLines
    applyRules rules ("in", fullFranges)
    |> List.sumBy rangeSize
