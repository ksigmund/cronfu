namespace CronFu

[<AutoOpen>]
module Cron =

    type CronTime<'a> =
        | Wildcard
        | Value of int
        | Range of Range<'a>

    type CronExpression =
        { Minutes: CronTime<Minute> seq
          Hours: CronTime<Hour> seq
          DaysOfMonth: CronTime<DayOfMonth> seq
          Months: CronTime<Month> seq
          DaysOfWeek: CronTime<DayOfWeek> seq }

    let toString (cronTimes: seq<CronTime<'a>>) =
        cronTimes
        |> Seq.mapi (fun i time ->
            match time with
            | Wildcard -> $"Every {typeof<'a>.Name}"
            | Value value -> $"Every {typeof<'a>.Name} {value}"
            | Range range -> $"From {typeof<'a>.Name} {range.From} to {range.To}")
        |> String.concat " and "

    let describe (result: Result<CronExpression, string>) =
        match result with
        | Ok cron -> cron.Minutes |> toString
        | Error err -> err
