namespace CronFu

[<AutoOpen>]
module Cron =

    open Time

    type CronTime<'a> =
        | Wildcard
        | Value of int
        | Range of Range<'a>
        override this.ToString() =
            match this with
            | Wildcard -> $"Every {typeof<'a>.Name}"
            | Value value -> $"Every {typeof<'a>.Name} {value}"
            | Range range -> $"From {typeof<'a>.Name} {range.From} to {range.To}"

    module CronTime =
    /// Creates a CronTime instance if the provided int value is between the allowed min and max values for the time type.
        let inline create<'a when 'a: (static member Min: int) and 'a: (static member Max: int)> : int -> Result<CronTime<'a>, string> =
            function
            | x when (x >= TimeMin<'a>) && (x <= TimeMax<'a>) -> Value x |> Result.Ok
            | _ -> Result.Error $"{typeof<'a>.Name} must be between {TimeMin<'a>}-{TimeMax<'a>}"

    let toString (cronTimes: CronTime<'a> seq) =
        cronTimes
        |> Seq.map (fun x -> x.ToString())
        |> String.concat " and "

    type CronExpression =
        { Minutes: CronTime<Minute> list
          Hours: CronTime<Hour> list
          DaysOfMonth: CronTime<DayOfMonth> list
          Months: CronTime<Month> list
          DaysOfWeek: CronTime<DayOfWeek> list }
        member this.toString =
            [ this.Minutes |> toString
              this.Hours |> toString
              this.DaysOfMonth |> toString
              this.Months |> toString
              this.DaysOfWeek |> toString ]
            |> String.concat " and "

        static member create(m, h, dom, month, dow) =
            { Minutes = m
              Hours = h
              DaysOfMonth = dom
              Months = month
              DaysOfWeek = dow }

     let describe (result: Result<CronExpression, string>) =
            match result with
            | Ok cron -> cron.toString
            | Error err -> err
