namespace CronFu

[<AutoOpen>]
module Cron =

    open Time

    type CronTime<'a> =
        | Wildcard
        | Value of int
        | Range of Range<'a>
        static member toString<'a> (cronTime: CronTime<'a>) =
            match cronTime with
            | Wildcard -> $"Every {typeof<'a>.Name}"
            | Value value -> $"Every {typeof<'a>.Name} {value}"
            | Range range -> $"From {typeof<'a>.Name} {range.From} to {range.To}"
        override this.ToString() = this |> CronTime<'a>.toString

    module CronTime =
    /// Creates a CronTime instance if the provided int value is between the allowed min and max values for the time type.
        let inline create<'a when 'a: (static member Min: int) and 'a: (static member Max: int)> : int -> Result<CronTime<'a>, string> =
            function
            | x when (x >= TimeMin<'a>) && (x <= TimeMax<'a>) -> Value x |> Ok
            | _ -> Error $"{typeof<'a>.Name} must be between {TimeMin<'a>}-{TimeMax<'a>}"

    let toString (cronTimes: CronTime<'a> seq) =
        cronTimes
        |> Seq.map CronTime.toString
        |> String.concat " and "

    type CronMinute = CronTime<Minute>
    type CronHour = CronTime<Hour>
    type CronDayOfMonth = CronTime<DayOfMonth>
    type CronMonth = CronTime<Month>
    type CronDayOfWeek = CronTime<DayOfWeek>

    type CronExpression =
        { Minutes: CronMinute list
          Hours: CronHour list
          DaysOfMonth: CronDayOfMonth list
          Months: CronMonth list
          DaysOfWeek: CronDayOfWeek list }
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
