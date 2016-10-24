namespace QuantFin

open Deedle
open Util
open YahooFinance

module Portfolio =

  type Portfolio = (float * Map<string, int>)

  /// <summary>
  /// Calculates the value of a portfolio given a map of security name to price
  /// </summary>
  /// <param name="port"></param>
  /// <param name="prices"></param>
  let portValue (port: Portfolio) (prices:Map<string, float>) =
    let (cash, positions) = port
    positions |> Map.fold (fun acc i pos -> acc + prices.[i] * float pos) cash

  /// <summary>
  /// Gets the position of a security from a portfolio
  /// </summary>
  /// <param name="port"></param>
  /// <param name="security"></param>
  let getPosition (port:Portfolio) security =
    let (_, positions) = port
    match Map.tryFind security positions with
    | Some pos -> pos
    | None -> 0

  /// <summary>
  /// Gets all the security names in a portfolio
  /// </summary>
  /// <param name="positions"></param>
  let getNames positions =
    positions |> Map.toSeq |> Seq.map fst |> Seq.toList

  /// <summary>
  /// Gets the historical price of a security from the cache.  Throws an
  /// exception if the cache doesn't have the security or the date is not in
  /// the cache
  /// </summary>
  /// <param name="cacheDir"></param>
  /// <param name="dt"></param>
  /// <param name="stockCode"></param>
  let histDate cacheDir dt stockCode : float =
    histCache cacheDir stockCode
    |> fst
    |> Frame.getCol "Close"
    |> Series.get dt

  /// <summary>
  /// Mark positions to market given a map of security to position and a map of
  /// security to price
  /// </summary>
  /// <param name="positions"></param>
  /// <param name="prices"></param>
  let mtmPrices (positions: Map<string, int>) (prices: Map<string, float>) =
    positions
    |> Map.map (fun stock pos -> prices.[stock] * (float pos))

  /// <summary>
  /// Retrives latest price from Yahoo Finance and mark a portfolio to market
  /// </summary>
  /// <param name="positions"></param>
  let mtm positions = 
    positions |> getNames |> quotes |> split (mtmPrices positions)

  /// <summary>
  /// Assume prices are in a cache, this function marks the positions to market
  /// on a particular date in the past.  If any price is missing in the cache
  /// for a particular security on the specified date, an exception is thrown
  /// </summary>
  /// <param name="cacheDir"></param>
  /// <param name="dt"></param>
  /// <param name="positions"></param>
  let mtmHist cacheDir dt positions =
    positions
    |> getNames
    |> Seq.map (fun s -> s, s |> histDate cacheDir dt )
    |> Map.ofSeq
    |> split (mtmPrices positions)


