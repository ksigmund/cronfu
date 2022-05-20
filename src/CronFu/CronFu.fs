module CronFu
    let parse = CronFu.Parsing.parse
    let describe = parse >> CronFu.Cron.describe
