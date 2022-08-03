#r "/workspaces/cronfu/src/CronFu/bin/Debug/net6.0/CronFu.dll"
#I "/workspaces/cronfu/build/bin/Debug/net6.0"

open System
open CronFu

Console.WriteLine (parseAndDescribe "5-11/2 4 1 * MON-FRI")
