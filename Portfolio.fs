namespace Fennel

open Deedle
open Util
open YahooFinance

module Portfolio =

  type Portfolio = (float * Map<string, int>)

  /// <summary>
  /// Creates a cash portfolio of the specified amount
  /// </summary>
  /// <param name="amt"></param>
  let cash amt : Portfolio = (amt, Map.empty)

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

  /// <summary>
  /// Updates the portfolio when a trade happens assuming a self-financing
  /// portfolio.  Or if either cash or position of the security is not
  /// sufficient to effect the trade, an exception is thrown
  /// </summary>
  /// <param name="security"></param>
  /// <param name="price"></param>
  /// <param name="qty"></param>
  /// <param name="port"></param>
  let trade security price qty (port: Portfolio) =
    let (cash, positions) = port
    let cash' = cash - (float qty) * price
    let pos' = match Map.tryFind security positions with
               | Some pos -> pos + qty
               | None -> qty
    if cash' < 0.0 then failwith (sprintf "Not enough cash to long %d units of %A@%f" qty security price)
    if pos' < 0 then failwith (sprintf "Position %d not enough to cover qty. Long only" (pos'+qty))
    cash - (float qty) * price, positions |> Map.add security pos'


