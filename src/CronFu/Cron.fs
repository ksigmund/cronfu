
[<AutoOpen>]
module CronFu.Cron
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


let toString (cronTimes: CronTime<'a> seq) =
    cronTimes
    |> Seq.map (fun x -> x.ToString())
    |> String.concat " and "

type CronExpression =
    { Minutes: CronTime<Minute> seq
      Hours: CronTime<Hour> seq
      DaysOfMonth: CronTime<DayOfMonth> seq
      Months: CronTime<Month> seq
      DaysOfWeek: CronTime<DayOfWeek> seq }
    member this.toString =
        [ this.Minutes |> toString
          this.Hours |> toString
          this.DaysOfMonth |> toString
          this.Months |> toString
          this.DaysOfWeek |> toString ]
        |> String.concat " and "
    static member create (m, h, dom, month, dow) =
        { Minutes = m
          Hours = h
          DaysOfMonth = dom
          Months = month
          DaysOfWeek = dow }

module CronExpression =

    let toString (result: Result<CronExpression, string>) =
        match result with
        | Ok cron -> cron.toString
        | Error err -> err
