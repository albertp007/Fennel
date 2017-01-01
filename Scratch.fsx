#load "packages/Deedle.1.2.5/Deedle.fsx"
#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"
#r @"MathNet.Numerics.Data.Text.dll"

open System
open System.IO
open Deedle
open QuantFin.Signal
open QuantFin.YahooFinance
open FSharp.Data
open QuantFin.Util
open QuantFin.Portfolio
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics
open QuantFin.ML

Control.NativeProviderPath <- Path.Combine [|__SOURCE_DIRECTORY__; @"bin\Debug"|]
Control.UseNativeMKL();;

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
  let prices = hist histOnline "cache" stock 
               |> Frame.filterRows (fun d _ -> d >= fromDate)
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
  |> List.map (hist histOnline "cache")

let getPrices histFunc name =
  hist histFunc "cache" name

let makeFeatures (p: Frame<DateTime, string>) =
  let logClose = p?Close |> log
  let logReturn = logClose - (logClose |> Series.shift 1)
  let makeReturn n =
    (sprintf "Return%0d" n, logReturn |> Series.shift n)
  let range = ("Range", (p?High - p?Low)/p?Close)
  let gap = ("Gap", (p?Close - p?Open)/p?Close)
  let rsiSeries = ("RSI", p?Close |> rsi 5)
  let volumeSeries = ("Volume", p?Volume)
  let macdSeries = ("MACD", p?Close |> macd 5 3 3)
  let y0 =
    p?Close - (p?Close |> Series.shift (-1))
    // p?Close - p?Open   // as a test, uncomment this line to get 100%
    |> Series.map (fun _ v -> if v > 0.0 then 1.0 else 0.0)
    // |> Series.shift (-1)
  [ rsiSeries
    volumeSeries
    range
    gap
    macdSeries
    makeReturn 0
    makeReturn 1
    makeReturn 2
    makeReturn 3
    makeReturn 4
    ("y", y0)
  ]
  |> Frame.ofColumns
  |> Frame.dropSparseRows
  |> Frame.toArray2D
  |> DenseMatrix.ofArray2

let lambda = 0.0
let epsilon = 0.001
let hidden = [9]
let stock = "^HSI"
let trainingPerc = 0.8
let testPerc = 0.2
let tolerance = 0.00001

stock
|> getPrices histCache 
|> makeFeatures 
|> runNN hidden lambda epsilon trainingPerc testPerc tolerance true true