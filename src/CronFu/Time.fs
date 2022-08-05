module CronFu.Time

open System

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

/// Month values
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

/// Day Values
type DayVal =
    | Sun = 0
    | Mon = 1
    | Tue = 2
    | Wed = 3
    | Thu = 4
    | Fri = 5
    | Sat = 6

/// Active pattern that returns Some if the provided string is a valid day, otherwise None.
let (|ValidDay|) (arg: string) =
    match Enum.TryParse<DayVal>(arg, true) with
    | (true, value) -> Some((int32) value)
    | _ -> None

// Minute type with a range of 0-59
type Minute = Minute
    with
        static member Min = 0
        static member Max = 59

// Hour type with a range of 0-23
type Hour = Hour
    with
        static member Min = 0
        static member Max = 23

// DayOfMonth type with a range of 1-31
type DayOfMonth = DayOfMonth
    with
        static member Min = 1
        static member Max = 31

// Month type with a range of 1-12
type Month = Month
    with
        static member Min = (int) MonthVal.Jan
        static member Max = (int) MonthVal.Dec
        static member IsValid str = (|ValidMonth|) str

// DayOfWeek type with a range of 0-6
type DayOfWeek = DayOfWeek
    with
        static member Min = (int) DayVal.Sun
        static member Max = (int) DayVal.Sat
        static member IsValid str = (|ValidDay|) str

let inline TimeMin<'T when 'T: (static member Min: int)> =
    (^T: (static member Min: int) ())

let inline TimeMax<'T when 'T: (static member Max: int)> =
    (^T: (static member Max: int) ())

let inline IsValid<'T when 'T: (static member IsValid: string -> option<int32>)> str =
    (^T: (static member IsValid: string -> option<int32>) (str))
