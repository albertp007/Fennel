#load "packages/Deedle.1.2.5/Deedle.fsx"
#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"
#r @"MathNet.Numerics.dll"

open System
open Deedle
open QuantFin.Signal
open QuantFin.YahooFinance
open FSharp.Data
open QuantFin.Util
open QuantFin.Portfolio

let genSignals n1 n2 (prices: Frame<DateTime, string>) =
  let blah = prices?Close
  let maNear = prices?Close |> sma n1
  let maFar = prices?Close |> sma n2
  let diff = maNear - maFar |> Series.dropMissing
  diff  
  |> Series.pairwiseWith (fun _ (d1, d2) -> sign(d1) <> sign(d2), sign(d2))
  // Look for change of sign, which is exactly when a cross happens
  |> Series.filterValues (fun (a, _) -> a)
  // -1 is a sell signal, therefore we want to skip all the -1 entries from
  // the beginning of the series until we find the first +1
  |> Series.skipWhile (fun _ (_, b) -> b = -1)
  |> Series.map (fun _ (_, sgn) -> sgn)
  
let allocate lotsize (price: float) (cash: float) =
  cash / price / (float lotsize) |> int |> (*) lotsize

let initPortfolio cash : Portfolio = (cash, Map.empty)

let genTrade security lotsize (portfolio: Portfolio) (qty, price) =
  let cash, positions = portfolio
  let qty' = 
    if qty > 0 then allocate lotsize price cash
    else qty * (getPosition portfolio security)
  trade security price qty' portfolio
    
let strategy0 stock lotsize initCash fromDate n1 n2 =
  let prices = hist "cache" stock |> Frame.filterRows (fun d _ -> d >= fromDate)
  let signals = genSignals n1 n2 prices
  let prices' =
    signals
    |> Series.keys  // get the date of the signals
    |> Frame.getNthRowAfterKeys 1 "Date" prices
  prices'?Open
  |> Series.zipInner signals
  |> Series.scanValues (fun port signal -> genTrade stock lotsize port signal)
       (initPortfolio initCash)
  |> Frame.addCol "Portfolio" <| prices'

let positions =
  [("0003.HK", 3896)
   ("0005.HK", 3200)
   ("0006.HK", 5500)
   ("0345.HK", 6000)
   ("0408.HK", 50000)
   ("0658.HK", 10000)
   ("0688.HK", 10977)
   ("0939.HK", 32100)
   ("0941.HK", 3000)
   ("1299.HK", 1000)
   ("1800.HK", 10000)
   ("1831.HK", 63000)
   ("2020.HK", 55000)
   ("2208.HK", 6200)
   ("2628.HK", 15000)
   ("2638.HK", 12500)
   ("2669.HK", 3659)
   ("2823.HK", 8000)] |> Map.ofList


let strategyForName initCash date n1 n2 (name, lotsize) =
  strategy0 name lotsize initCash date n1 n2

let refreshCache positions =
  positions
  |> getNames
  |> List.map (hist "cache")

let linearRegress (x: Series<'a, float>) y =
  Series.zipInner x y
  |> Series.values
  |> MathNet.Numerics.LinearRegression.SimpleRegression.Fit
  