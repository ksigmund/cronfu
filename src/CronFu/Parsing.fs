[<AutoOpen>]
module CronFu.Parsing

open Time
open Cron
open FParsec

type CronParser<'a> = Parser<CronTime<'a>, unit>

let pOption f err =
    function
    | Some a -> preturn (f a)
    | None -> fail err

let pResult f =
    function
    | Result.Ok a -> preturn (f a)
    | Result.Error err -> fail err

let valueTime (cronValue: CronTime<_>) _ =
    match cronValue with
    | Value v -> Reply(v)
    | _ -> Reply(Error, expectedString "Value must be an integer.")

/// Returns a parser for wildcards (i.e. the '*' character) which returns the Wildcard case when successful.
let pWildcard<'a> : CronParser<'a> = '*' |> pchar >>% Wildcard

let inline pStrTime<'a when 'a: (static member IsValid: string -> option<int32>)> =
    many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c) //"a better error message here"
    >>= (fun result ->
        (IsValid<'a> result)
        |> pOption id $"{result} is not a valid {typeof<'a>.Name}.")

let pTime (parser: Parser<'a, unit>) (create) = parser >>= (create >> (pResult id))

let pIntTime<'a> : (int32 -> Result<'a, string>) -> Parser<'a, unit> = pTime pint32

/// Creates a range parser from a low and a high value, as well as an optional step
let pTimeRange ((t1, t2), step) : CronParser<'a> =
    (Time.Range<'a>.tryCreate t1 t2 step)
    |> pResult Range

/// Create a parser that parses ranges (e.g. 14-21) with an optional step value (e.g. 1-30/5)
let pRange (pValueTime: Parser<int32, unit>) : CronParser<'a> =
    (pValueTime .>>? pstring "-"
     .>>.? pValueTime
     .>>.? opt (pstring "/" >>. pint32)
     >>= pTimeRange)
    <?> "Invalid range"

/// Accepts a CronParser and returns a parser that also parses a range (with optional step) and a wildcard for the same type
let cronParser<'a> (pCron: CronParser<'a>) =
    let pCronRange = pCron >>= valueTime |> pRange

    sepBy1
        (many1 (choiceL [ pCronRange; pCron; pWildcard ] "Each unit of time must be a valid range, value, or wildcard."))
        (skipChar ',') // it's critical that pCronRange is first in the list
    |>> List.collect id

// Create a parser that returns a list of CronTimes if the provided int value and ranges are valid
let inline pCronInt<'a when 'a: (static member Min: int) and 'a: (static member Max: int)> =
    CronTime.create |> pIntTime |> cronParser<'a>

// Create a parser that returns a list of CronTimes if the provided string value and ranges are valid
let inline pCronStr<'a when 'a: (static member IsValid: string -> option<int32>) and 'a: (static member Min: int) and 'a: (static member Max: int)> =
    CronTime.create<'a> |> (pTime pStrTime<'a>) |> cronParser

/// Creates a parser that returns a list of CronTimes, including ranges, individuals months, and wildcards for a CronTime that can either be an int or a string
let inline pCronIntStr<'a when 'a: (static member IsValid: string -> option<int32>) and 'a: (static member Min: int) and 'a: (static member Max:
    int)> =
    pCronInt <|> attempt (pCronStr<'a>)

/// A parser that produces a CronExpression
let pCronExpression =
    parse {
        let! minutes = (pCronInt .>> spaces1)
        let! hours = (pCronInt .>> spaces1)
        let! days = (pCronInt .>> spaces1)
        let! months = (pCronIntStr .>> spaces1)
        let! dow = (pCronIntStr .>> eof)
        return (minutes, hours, days, months, dow)
    }
    |>> CronExpression.create

let parse expression =
    let result = run pCronExpression expression
    match result with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
