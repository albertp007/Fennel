namespace QuantFin

open Deedle
open Util
open YahooFinance

module Portfolio =

  type Portfolio = (float * Map<string, int>)

  let portValue (port: Portfolio) (prices:Map<string, float>) =
    let (cash, positions) = port
    positions |> Map.fold (fun acc i pos -> acc + prices.[i] * float pos) cash

  let getPosition (port:Portfolio) security =
    let (_, positions) = port
    match Map.tryFind security positions with
    | Some pos -> pos
    | None -> 0

  let getNames positions =
    positions |> Map.toSeq |> Seq.map fst |> Seq.toList

  let histDate cacheDir dt stockCode : float =
    histCache cacheDir stockCode
    |> fst
    |> Frame.getCol "Close"
    |> Series.get dt

  let mtmPrices (positions: Map<string, int>) (prices: Map<string, float>) =
    positions
    |> Map.map (fun stock pos -> prices.[stock] * (float pos))

  let mtm positions = 
    positions |> getNames |> quotes |> split (mtmPrices positions)

  let mtmHist cacheDir dt positions =
    positions
    |> getNames
    |> Seq.map (fun s -> s, s |> histDate cacheDir dt )
    |> Map.ofSeq
    |> split (mtmPrices positions)


