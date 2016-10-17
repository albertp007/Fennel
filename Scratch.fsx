#load "packages/Deedle.1.2.5/Deedle.fsx"
#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"

open System
open Deedle
open QuantFin.Signal
open QuantFin.YahooFinance
open FSharp.Data

let anta = downloadHist "2020.HK"
let hsi = downloadHist "2800.HK"
