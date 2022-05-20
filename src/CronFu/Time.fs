namespace CronFu

[<AutoOpen>]
module Time =
    open System
    open System.ComponentModel

    // Represents a range of values, optionally separated by a an incremental step
    type Range<'a> =
        private
            { from: int
              ``to``: int
              step: int option }
        member this.From = this.from
        member this.To = this.``to``
        member this.Step = this.step

        static member tryCreate t1 t2 step =
            match (t1, t2) with
            | (t1, t2) when t1 <= t2 -> Ok { from = t1; ``to`` = t2; step = step }
            | _ -> Error $"Ranges must resprsent an ascending series of values. {t2} is less than {t1}."

    type MonthVal =
        | Jan = 1
        | Feb = 2
        | Mar = 3
        | Apr = 4
        | May = 5
        | Jun = 6
        | Jul = 7
        | Aug = 8
        | Sep = 9
        | Oct = 10
        | Nov = 11
        | Dec = 12

    /// Active pattern that returns Some if the provided string is a valid month, otherwise None.
    let (|ValidMonth|) (x: string) =
        match Enum.TryParse<MonthVal>(x, true) with
        | (true, value) -> Some((int32) value)
        | _ -> None

    type DayVal =
        | Sun = 0
        | Mon = 1
        | Tue = 2
        | Wed = 3
        | Thu = 4
        | Fri = 5
        | Sat = 6

    let (|ValidDay|) (arg: string) =
        match Enum.TryParse<DayVal>(arg, true) with
        | (true, value) -> Some((int32) value)
        | _ -> None

    [<DisplayName("Blah")>]
    type Minute = Minute
        with
            static member Min = 0
            static member Max = 59

    type Hour = Hour
        with
            static member Min = 0
            static member Max = 23

    type DayOfMonth = DayOfMonth
        with
            static member Min = 1
            static member Max = 31

    type Month = Month
        with
            static member Min = (int) MonthVal.Jan
            static member Max = (int) MonthVal.Dec

            static member IsValid str =
                match str with
                | ValidMonth (m) -> m

    type DayOfWeek = DayOfWeek
        with
            static member Min = 0
            static member Max = 6

            static member IsValid str =
                match str with
                | ValidDay (d) -> d

    let inline TimeMin<'T when 'T: (static member Min: int)> =
        (^T: (static member Min: int) ())

    let inline TimeMax<'T when 'T: (static member Max: int)> =
        (^T: (static member Max: int) ())

    let inline IsValid<'T when 'T: (static member IsValid: string -> option<int32>)> str =
        (^T: (static member IsValid: string -> option<int32>) (str))
