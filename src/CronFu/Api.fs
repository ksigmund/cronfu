[<AutoOpen>]
module CronFu.Api

// Compose the parse function with the describe function
let parseAndDescribe = Parsing.parse >> Cron.describe
