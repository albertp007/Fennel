#load "packages/Deedle.1.2.5/Deedle.fsx"
#load "packages/RProvider.1.1.20/RProvider.fsx"
#r @"bin\Debug\MathNet.Numerics.dll"
#r @"bin\Debug\MathNet.Numerics.FSharp.dll"
#r @"bin\Debug\MathNet.Numerics.Data.Text.dll"
#r @"bin\Debug\XPlot.Plotly.dll"
#r @"bin\Debug\XPlot.GoogleCharts.dll"
#r @"bin\Debug\XPlot.GoogleCharts.Deedle.dll"
#r @"bin\Debug\Deedle.RProvider.Plugin.dll"
#r @"bin\Debug\QuantFin.dll"

open System
open System.IO
open Deedle
open FSharp.Data
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics
open RDotNet
open RProvider
open RProvider.graphics
open RProvider.stats
open QuantFin.Util
open QuantFin.Portfolio
open QuantFin.Signal
open QuantFin.YahooFinance
open QuantFin.ML
open QuantFin.Plot

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
   ("2020.HK", 45000)
   ("2208.HK", 6200)
   ("2628.HK", 15000)
   ("2638.HK", 12500)
   ("2669.HK", 3659)
   ("2823.HK", 8000)] |> Map.ofList

let strategyForName initCash date n1 n2 (name, lotsize) =
  strategy0 name lotsize initCash date n1 n2

let refreshCacheByPosition positions =
  positions
  |> getNames
  |> List.map (hist histOnline "cache")

let refreshCache() =
  let cacheName = "cache"
  let r = 
    cacheName 
    |> sprintf @"^%s\\(?<name>[\^\.A-z0-9]*)\.csv$" 
    |> System.Text.RegularExpressions.Regex
  System.IO.Directory.GetFiles(cacheName, "*.csv")
  |> Seq.iter (fun s ->
                let m = r.Match(s)
                if m.Success then 
                  hist histOnline cacheName (m.Result("${name}")) |> ignore)

let getPrices histFunc name =
  hist histFunc "cache" name

let spx = getPrices histCache "^GSPC"
let n225 = getPrices histCache "^N225"

let makeFeaturesDataFrame (p: Frame<DateTime, string>) =
  let logClose = p?Close |> log
  let logReturn = logClose - (logClose |> Series.shift 1)
  let makeReturn n =
    (sprintf "Return%0d" n, logReturn |> Series.shift n)
  [ 
    ("RSI", p?Close |> rsi 5)
    ("Volume", p?Volume)
    // ("MACD", p?Close |> macd 5 3 3)
    ("Range", (p?High - p?Low)/p?Close)
    ("Gap", (p?Close - p?Open)/p?Close)
    // ("High0", p?High)
    // ("High1", p?High |> Series.shift 1)
    // ("High2", p?High |> Series.shift 2)
    // ("Low0", p?Low)
    // ("Low1", p?Low |> Series.shift 1)
    // ("Low2", p?Low |> Series.shift 2)
    // ("Open0", p?Open)
    // ("Open1", p?Open |> Series.shift 1)
    // ("Open2", p?Open |> Series.shift 2)
    // ("Close0", p?Close)
    // ("Close1", p?Close |> Series.shift 1)
    // ("Close2", p?Close |> Series.shift 2)
    ("SPX0", spx?Close - (spx?Close |> Series.shift 1))
    // n225 opens earlier than HK, thus we can use n225 open on the next day
    ("N225", (n225?Open |> Series.shift -1) - n225?Close)
    // ("SPX1", spx?Close |> Series.shift 1)
    makeReturn 0
    makeReturn 1
    makeReturn 2
    makeReturn 3
    makeReturn 4
    ("y", 
      // (p?Close |> Series.shift (-1)) - p?Close
      (p?Close - p?Open) |> Series.shift -1
      // p?Close - p?Open   // as a test, uncomment this line to get 100%
      |> Series.map (fun _ v -> if v > 0.0 then 1.0 else 0.0))
  ]
  |> Frame.ofColumns
  |> Frame.dropSparseRows

let makeFeatures (p: Frame<DateTime, string>) =
  p
  |> makeFeaturesDataFrame
  |> Frame.toArray2D
  |> DenseMatrix.ofArray2

let lambda = 0.0
let epsilon = 0.001
let hidden = [20]
let stock = "^HSI"
let trainingPerc = 0.8
let testPerc = 0.2
let tolerance = 0.00001

stock
|> getPrices histCache 
|> makeFeatures 
|> runNN hidden lambda epsilon trainingPerc testPerc tolerance true true

open RProvider.tseries
let features = getPrices histCache "^HSI" |> makeFeaturesDataFrame
let res = R.adf_test(features?RSI)
let air = RProvider.datasets.R.AirPassengers
let s = R.start(air)
let summary = R.summary(s)
R.plot
