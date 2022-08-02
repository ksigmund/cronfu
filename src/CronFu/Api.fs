[<AutoOpen>]
module CronFu.Api

let describe = parse >> CronExpression.toString
