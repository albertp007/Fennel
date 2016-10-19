#load "packages/Deedle.1.2.5/Deedle.fsx"
#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"

open System
open Deedle
open QuantFin.Signal
open QuantFin.YahooFinance
open FSharp.Data
open QuantFin.Util

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

let strategy stock n1 n2 =
  let prices = hist "cache" stock
  let signals = genSignals n1 n2 prices
  let prices' =
    signals
    |> Series.keys  // get the date of the signals
    |> Frame.getNthRowAfterKeys 1 "Date" prices
  prices'?Open
  |> Series.zipInner signals
   
type Portfolio<'i when 'i : comparison> = (float * Map<'i, int>)
let portValue (port: Portfolio<'i>) (prices:Map<'i, float>) =
  let (cash, positions) = port
  positions |> Map.fold (fun acc i pos -> acc + prices.[i] * float pos) cash

let trade security price qty (port: Portfolio<'i>) =
  let (cash, positions) = port
  let cash' = cash - (float qty) * price
  let pos' = match Map.tryFind security positions with
             | Some pos -> pos + qty
             | None -> qty
  if cash' < 0.0 then failwith (sprintf "Not enough cash to long %d units of %A@%f" qty security price)
  if pos' < 0 then failwith (sprintf "Position %d not enough to cover qty. Long only" (pos'+qty))
  cash - (float qty) * price, positions |> Map.add security pos'

let allocate lotsize (price: float) (cash: float) =
  cash / price / (float lotsize) |> int |> (*) lotsize
