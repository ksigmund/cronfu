namespace CronFu

[<AutoOpen>]
module Parsing =
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

    let valueTime (cronValue: CronTime<'a>) _ =
        match cronValue with
        | Value v -> Reply(v)
        | _ -> Reply(Error, expectedString "Value must be an integer.")

    /// Creates a CronTime instance if the provided int value is between the allowed min and max values for the time type.
    let inline create<'a when 'a: (static member Min: int) and 'a: (static member Max: int)> : int
        -> Result<CronTime<'a>, string> =
        function
        | x when (x >= TimeMin<'a>) && (x <= TimeMax<'a>) -> Value x |> Result.Ok
        | _ -> Result.Error $"{typeof<'a>.Name} must be between {TimeMin<'a>}-{TimeMax<'a>}"

    /// Returns a parser for wildcards (i.e. the '*' character) which returns the Wildcard case when successful.
    let pWildcard<'a> : CronParser<'a> = pchar '*' >>% Wildcard

    let inline pTimeStr<'a when 'a: (static member IsValid: string -> option<int32>)> =
        many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c) //"a better error message here"
        >>= (fun result ->
            (IsValid<'a> result)
            |> pOption id $"{result} is not a valid {typeof<'a>.Name}.")

    let pTime (parser: Parser<'a, unit>) (create) = parser >>= (create >> pResult id)

    let pIntTime<'a> : (int32 -> Result<'a, string>) -> Parser<'a, unit> = pTime pint32

    /// Creates a range parser from a low and a high value, as well as an optional step
    let pTimeRange ((t1, t2), step) : CronParser<'a> =
        (Time.Range<'a>.tryCreate t1 t2 step)
        |> pResult Range

    /// Create a parser that parses ranges (e.g. 14-21 or 1-30/5)
    let pRange (pValueTime: Parser<int32, unit>) : CronParser<'a> =
        (pValueTime .>>? pstring "-"
         .>>.? pValueTime
         .>>.? opt (pstring "/" >>. pint32)
         >>= pTimeRange)
        <?> "Invalid range"

    /// Takes a CronParser and returns a parser that also parses a range (with optional step) and a wildcard for the same type
    let cronParser<'a> (pCron: CronParser<'a>) =
        let pCronRange = pCron >>= valueTime |> pRange

        sepBy1
            (many1 (
                choiceL [ pCronRange; pCron; pWildcard ] "Each unit of time must be a valid range, value, or wildcard."
            ))
            (skipChar ',') // it's critical that pCronRange is first in the list
        |>> List.collect id

    /// Creates a parser that returns a list of CronMonths, including ranges, individuals months, and wildcards
    let inline pCronIntStr<'a when 'a: (static member IsValid: string -> option<int32>) and 'a: (static member Min: int) and 'a: (static member Max:
        int)> =
        choice [ (pIntTime create)
                 attempt (pTime pTimeStr<'a> create) ]
        |> cronParser<'a>

    let inline pCronInt<'a when 'a: (static member Min: int) and 'a: (static member Max: int)> =
        create |> pIntTime |> cronParser<'a>

    let pCronExpression =
        tuple5
            (pCronInt .>> spaces1)
            (pCronInt .>> spaces1)
            (pCronInt .>> spaces1)
            (pCronIntStr .>> spaces1)
            (pCronIntStr .>> eof)
        |>> (fun (m, h, dom, month, dow) ->
            { Minutes = m
              Hours = h
              DaysOfMonth = dom
              Months = month
              DaysOfWeek = dow })

    let parse expression =
        match run pCronExpression expression with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err
