namespace QuantFin

open Deedle
open System
open NUnit.Framework
open FsUnit
open Portfolio

module Signal =

  type PriceSeries = Series<DateTime, float>

  type TechnicalIndicator =
    | Rsi of int
    | Sma of int
    | Ema of int
    | Macd of int * int * int
    | Stochastic of int * int * int

  /// <summary>This function calculates the dot product of two lists of
  /// float numbers </summary>
  /// <param name="v1">the first vector of floats</param>
  /// <param name="v2">the second vector of floats</param>
  /// <returns>The dot product of v1 and v2</returns>
  ///
  let dot v1 v2 =
    Seq.fold (fun a (x, y)->a + x*y) 0.0 (Seq.zip v1 v2)

  /// <summary>produces a list of lists which is the n-slot moving window of a
  /// sequence of float.  Then apply the function f to each list of n elements
  /// which are the elements in that window.  The resulting list is prepended
  /// with (n-1) zeroes to make it the same length of the original sequence
  /// </summary>
  /// <param name="n">length of the window</param>
  /// <param name="f">function to apply to each window</param>
  /// <returns>sequence of length equal to n, with the first n-1 elements
  /// being 0.0; thereafter, the element is the result of applying f to each
  /// window</returns>
  ///
  let filterByFunc n f lst =
    lst |> Seq.windowed n
        |> Seq.map f
        |> Seq.append (Seq.init (n-1) (fun _ -> 0.0))

  /// <summary>This function is a simplified version of the filter() function
  /// in R. </summary>
  /// <param name="w">Vector of weights (float)</param>
  /// <param name="lst">The list of float values</param>
  /// <returns>The series of numbers w0x0+w1x1+...+wnxn,
  /// w0x1+w1x2+...+wnx(n+1) ...</returns>
  ///
  let filter w lst =
    filterByFunc (Seq.length w) (dot w) lst

  /// <summary>This function calculates the simple moving average of a list of
  /// float values</summary>
  /// <param name="n">Size of the moving window</param>
  /// <param name="lst">The list of float values</param>
  /// <returns>The first n-1 values will be 0. The values onwards are the
  /// moving average calculated at the time point n</returns>
  ///
  let movingAverage n lst =
    filterByFunc n (Seq.average<float>) lst |> Seq.toList

  /// <summary>
  /// Similar to the unzip function in the List module, this function returns
  /// a pair of series given a series of pairs
  /// </summary>
  /// <param name="s"></param>
  let unzip (s: Series<'k, 'a*'b>) =
    let keys = s |> Series.keys
    s
    |> Series.values
    |> Seq.toList
    |> List.unzip
    |> fun (l1, l2) -> Series.ofValues l1 |> Series.indexWith keys,
                       Series.ofValues l2 |> Series.indexWith keys

  /// <summary> Calculates simple moving average of the close price given
  /// a list of bars </summary>
  /// <param name="n">Size of the moving window</param>
  /// <param name="prices">list of bars</param>
  /// <returns>list of floats</returns>
  ///
  let sma n (prices: PriceSeries) = 
    prices |> Stats.movingMean n |> Series.dropMissing

  /// <summary>
  /// Folder function for calculating ema by using the formula
  ///
  /// E(n) = alpha * P(n) + (1 - alpha) * E(n-1)
  ///
  /// E(n) is then pushed to the head of the list which is the accumulator. In
  /// the next iteration, the head of the list is E(n-1) and can be accessed in
  /// constant time.  A consequence is that the accumlator contains E(n) in
  /// reverse order and the end result needs to be reversed once to make the
  /// ordering correct
  /// </summary>
  /// <param name="alpha"></param>
  /// <param name="acc">List of E(n)</param>
  /// <param name="price">Current price</param>
  let emaFold alpha acc price =
    match acc with
    | [] -> failwith "Initial value cannot be empty. Usd SMA as the first element"
    | h::_ as acc' -> (alpha * price + (1.0 - alpha)*h)::acc'

  /// <summary>
  /// Calculates EMA given a function for calculating alpha from the number of
  /// periods.  
  /// </summary>
  /// <param name="f"></param>
  /// <param name="n"></param>
  /// <param name="prices"></param>
  let emaAlpha f n prices =
    // e0 is taken to be the simple mean of the first n elements
    let e0 = prices |> Series.take n |> Stats.mean
    // e0 will be included in the resulting list of numbers and to turn the
    // list back into a deedle series, we need to get the keys from the original
    // price series. e.g. if n = 10, the first 9 elements don't have an EMA. e0
    // starts at the 10th element.  Therefore, skip the first 9
    let keys = prices |> Series.skip (n-1) |> Series.keys
    let alpha = f n
    prices
    |> Series.skip n  // skipping n here because the e0 is the nth one
    |> Series.foldValues (emaFold alpha) [e0]
    |> List.rev       // reverse the list here because of the way the fold works
    |> Series.ofValues
    |> Series.indexWith keys

  /// <summary>
  /// Calculates the "regular" EMA with alpha defined to be 2/(1 + n) where n
  /// is the number of periods
  /// </summary>
  /// <param name="n"></param>
  /// <param name="prices"></param>
  let ema n prices = emaAlpha (fun n -> 2.0/(1.0 + float n)) n prices

  /// <summary>
  /// Calculates the "RSI" EMA where alpha is defined to be 1/n where n is the
  /// number of periods
  /// </summary>
  /// <param name="n"></param>
  /// <param name="prices"></param>
  let emaRsi n prices = emaAlpha (fun n -> 1.0/float n) n prices

  /// <summary>Calculates n day RSI given a list of bars</summary>
  /// <param name="n">number of days</param>
  /// <param name="prices">list of QuantFin.Data.Bars</param>
  /// <returns>list of RSI with the first n entries being zero</returns>
  ///
  let rsi n (prices: PriceSeries) =
    prices 
    |> Series.pairwise
    |> Series.mapValues (
        fun (c0, c1) -> 
          let delta = c1 - c0
          if delta >= 0.0 then (delta, 0.0) else (0.0, abs delta)
          )
    |> unzip
    |> fun (seriesU, seriesD) ->
         Series.zipInner (seriesU |> emaRsi n) (seriesD |> emaRsi n)
    |> Series.mapValues (
         fun (mau, mad) ->
           let rs = if mad <= 0.0 then 0.0 else mau/mad
           100.0 - 100.0/(1.0 + rs)
        )
   
  /// <summary>
  /// Calculates MACD
  /// </summary>
  /// <param name="n1">number of periods for macd</param>
  /// <param name="n2">number of periods for macd</param>
  /// <param name="s">number of periods for the smoothing signal line</param>
  /// <param name="prices">deedle series of prices</param>
  let macd n1 n2 s (prices: PriceSeries) =
    let (n1', n2') = if (n1 > n2) then (n2, n1) else (n1, n2)
    let emaN1 = prices |> ema n1'
    let emaN2 = prices |> ema n2'
    let macd = emaN1 - emaN2 |> Series.skip (n2' - n1') // drops missing
    let signal = macd |> ema s
    let divergence = macd - signal
    divergence  |> Series.dropMissing

  /// <summary>
  /// Calculates Slow Stochastics.  Unlike other technical indicator calculator
  /// function, this function returns a data frame instead of just one single
  /// series
  /// </summary>
  /// <param name="n1"></param>
  /// <param name="n2"></param>
  /// <param name="n3"></param>
  /// <param name="prices"></param>
  let stochastic n1 n2 n3 (prices: PriceSeries) =
    let percK = 
      prices
      |> Series.window n1
      |> Series.map (fun _ w -> w |> Stats.min, w |> Stats.max, 
                                w |> Series.lastValue)
      |> Series.map (
           fun _ (min, max, close) -> 
             match (min, max) with
             | Some min', Some max' -> (close - min')/(max' - min')
             | _ -> failwith "Weird, no min max found" )
    let percD = percK |> sma n2
    let percSlowD = percD |> sma n3
    [percD; percSlowD]

  /// <summary>
  /// This function defines the name of each of the series returned by the
  /// calculation of all the technical indicators
  /// </summary>
  /// <param name="t"></param>
  /// <returns>List of names of each of the series of an indicator</returns>
  let tiName t =
    match t with
    | Sma n -> [sprintf "SMA_%d" n]
    | Ema n -> [sprintf "EMA_%d" n]
    | Rsi n -> [sprintf "RSI_%d" n]
    | Macd (a, b, c) -> [sprintf "MACD_%d_%d_%d" a b c]
    | Stochastic (a, b, c) -> [sprintf "STOCH_%d_%d_%d_D" a b c;
                               sprintf "STOCH_%d_%d_%d_SlowD" a b c]

  /// <summary>
  /// Create a data frame given a list of names and a list of series. Both lists
  /// must be of the same size.  Each of the element in the name list
  /// corresponds to the column name of each of the column in the column list
  /// </summary>
  /// <param name="colNames"></param>
  /// <param name="seriesList"></param>
  let makeFrameFromSeries colNames seriesList =
    List.zip colNames seriesList
    |> Frame.ofColumns
        
  /// <summary>Calculates different types of technical indicators given a list
  /// of bars </summary>
  /// <param name="t">the technical indicator of type TechnicalIndicator</param>
  /// <param name="prices">the list of price bars</param>
  /// <returns>list of floats</returns>
  ///
  let calcTI t (prices: PriceSeries) =
    let names = t |> tiName
    match t with
    | Sma n -> [sma n prices]
    | Ema n -> [ema n prices]
    | Rsi n -> [rsi n prices]
    | Macd (a, b, c) -> [macd a b c prices]
    | Stochastic (a, b, c) -> stochastic a b c prices // func returns a list

  /// <summary>
  /// Merged the list of series which is the result of the calculation of the
  /// specified technical indicators into the original price data frame
  /// </summary>
  /// <param name="t"></param>
  /// <param name="prices"></param>
  let mergeTI t (prices: PriceSeries) =
    prices
    |> calcTI t
    |> makeFrameFromSeries (tiName t)

  /// <summary>Augments the price data frame with one technical indicator using
  /// the series specified in the 'field' argument
  /// </summary>
  /// <param name="field">pass the series in the column with the name specified
  /// in field to the ti calculation function</param>
  /// <param name="ti">the technical indicator of type TechnicalIndicator
  /// </param>
  /// <param name="prices">the list of price bars</param>
  /// <returns>list of tuple of price bar and float which is the value of the
  /// technical indicator calculated</returns>
  ///
  let augmentWith field ti priceFrame = 
    priceFrame
    |> Frame.getCol field
    |> mergeTI ti
    |> Frame.join JoinKind.Left priceFrame

  /// <summary>
  /// Augment the price data frame with one technical indicator using the 
  /// close price column in the data frame, assuming it exists. Otherwise, it
  /// throws an exception
  /// </summary>
  /// <param name="ti"></param>
  /// <param name="priceFrame"></param>
  let augment ti priceFrame = augmentWith "Close" ti priceFrame

  /// <summary>Type representing the interface of a signal generator expected
  /// by the findPattern function</summary>
  /// <param name="augment">the augment function to augment the list of price
  /// bars expected by the function defined by the find function</param>
  /// <param name="find">the function used to check whether the list of price
  /// bars augmented by the augment function contains a pattern or signal at
  /// the head of the list</param>
  ///
  type ISignalGenerator<'A, 'S> =
    abstract augment : PriceSeries -> 'A list
    abstract find : 'A list -> int -> 'S option

  /// <summary>The find function defines the bearish engulfing candlestick
  /// pattern.  The augment function augments the list of prices by RSI
  /// </summary>
  /// <param name="pList">list of price bars to process</param>
  /// <param name="augList">list of augmented price bars</param>
  /// <returns>optional pair of System.DateTime indicating the start time and
  /// end time of the pattern if found</returns>
  ///
//  let bearishEngulf = {
//    new ISignalGenerator<_,_> with
//      member this.augment pList = augment (Rsi 14) pList
//      member this.find augList n =
//        match augList with
//        | (h1, rsi1)::(h2, rsi2)::_ ->
//            if h2.o > h1.c && h2.c < h1.o &&
//              h1.o < h1.c && rsi1 > 80.0 then
//              Some (h1.d, h2.d, rsi1, rsi2, n+2) else None
//        | _ -> None
//  }

  /// <summary>This function finds the occurrence of price patterns as defined
  /// by the function f from the list of price data</summary>
  /// <param name="f">the function which takes a list of price bars and
  /// returns an optional pair of (System.DataTime*System.DateTime) to
  /// indicate the date time of the start of the pattern and the end of the
  /// pattern</param>
  /// <param name="prices">the list of price bars in which patterns are to be
  /// found</param>
  /// <returns>list of (System.DateTime*System.DateTime) pairs indicating the
  /// start and end date of all occurrences of the pattern</returns>
  ///
  let findPattern (sigGen: ISignalGenerator<_,_>) prices =
    let rec findPatternHelp f acc n pList =
      match pList with
      | [] -> List.rev acc
      | h::t ->
        match f pList n with
        | Some startEndDate -> findPatternHelp f (startEndDate::acc) (n+1) t
        | None -> findPatternHelp f acc (n+1) t
    sigGen.augment prices |> findPatternHelp sigGen.find [] 0

  type Signal = Entry | Exit | Hold

  /// <summary>
  /// A crossover function meant to be generic and generate a series of signals
  /// of +1 when the trigger line crosses the signal line from below (+ implies
  /// a signal to long) and -1 when the trigger line crosses the signal line 
  /// from above (- implies a signal to short).  The date in the signal series
  /// is the date when the cross happens
  /// </summary>
  /// <param name="trigger">A function which takes a price data frame
  /// and return a series which represents the trigger line</param>
  /// <param name="signal">A function which takes a price data frame and return
  /// a series which represents the signal line</param>
  /// <param name="prices"></param>
  let crossover (trigger: Frame<DateTime,string>->Series<DateTime,float>) signal
    prices =
      trigger prices - signal prices
      |> Series.dropMissing
      |> Series.pairwiseWith (fun _ (d1, d2) -> sign d1 = sign d2, sign d2)
      |> Series.filterValues (fun (sameSign, _) -> not sameSign)
      |> Series.skipWhile (fun _ (_, sgn) -> sgn < 0)
      |> Series.mapValues snd

  type OscValue = Oversold | Neutral | Overbought
  type OscState = Init | OversoldFound | NeutralFound

  let foldOsc (state, acc) (d, v) =
    match state with
    | Init ->
        match v with
        | Oversold -> (OversoldFound, acc)
        | _ -> (Init, acc)
    | OversoldFound ->
        match v with
        | Oversold -> (OversoldFound, acc)
        | Neutral -> (NeutralFound, (d, v)::acc)
        | Overbought -> (Init, (d,v)::acc)
    | NeutralFound ->
        match v with
        | Overbought -> (Init, (d, v)::acc)
        | _ -> (NeutralFound, acc)
      
  let oscillator (signal: Frame<DateTime,string>->Series<DateTime,'b>) lower
    upper prices =
      prices
      |> signal
      |> Series.map (fun k v -> 
        if v <= lower then k, Oversold else
        if v >= upper then k, Overbought else k, Neutral)
      |> Series.foldValues foldOsc (Init, [])
      |> snd
      |> series
      |> Series.sortByKey
      |> Series.map (fun _ v -> 
        match v with
        | Oversold -> 1
        | Neutral -> 1
        | Overbought -> -1 )  

  /// <summary>
  /// This function takes the dates in a series of signals and generate the
  /// price to enter into a trade using the open price of the next bar after
  /// the signal occurs
  /// </summary>
  /// <param name="prices"></param>
  /// <param name="signals"></param>
  let nextBarOpen prices signals : Series<DateTime, float> =
    signals
    |> Series.keys
    |> Frame.getNthRowAfterKeys 1 "Date" prices
    |> Frame.getCol "Open"

  /// <summary>
  /// This is a trade generation function which updates a particular portfolio
  /// with either a buy or a sell signal and the price of the security. If the
  /// signal is a buy signal i.e. qty = +1, it will buy as many round-lot shares
  /// as possible given the amount of cash in the portfolio.  Or if the qty is
  /// negative, it will sell the whole position of that security in the 
  /// portfolio
  /// </summary>
  /// <param name="security"></param>
  /// <param name="lotsize"></param>
  /// <param name="portfolio"></param>
  /// <param name="qty"></param>
  /// <param name="price"></param>
  let buyAllSellAll security lotsize (portfolio: Portfolio) (qty, price) =
    let allocate lotsize (price: float) (cash: float) =
      cash / price / (float lotsize) |> int |> (*) lotsize
    let cash, positions = portfolio
    let qty' = 
      if qty > 0 then allocate lotsize price cash
      else -(getPosition portfolio security)
    trade security price qty' portfolio

  /// <summary>
  /// A generic strategy function which takes a signal generator function, a
  /// price generator function and a trade generator function to process the
  /// initial portfolio with a price data frame, arriving at a series of
  /// portfolio at different points in time when a signal occurs
  /// </summary>
  /// <param name="sigGen"></param>
  /// <param name="priceGen"></param>
  /// <param name="tradeGen"></param>
  /// <param name="initPortfolio"></param>
  /// <param name="prices"></param>
  let strategy sigGen priceGen tradeGen initPortfolio prices =
    let signals = sigGen prices
    signals
    |> priceGen prices
    |> Series.zipInner signals
    |> Series.scanValues tradeGen initPortfolio

  /// <summary>
  /// A crossover strategy using a near term SMA as the trigger line and a
  /// longer term SMA as the signal line
  /// </summary>
  /// <param name="security"></param>
  /// <param name="lotsize"></param>
  /// <param name="n1"></param>
  /// <param name="n2"></param>
  /// <param name="initCapital"></param>
  /// <param name="prices"></param>
  let crossoverStrategy security lotsize n1 n2 initCapital prices =
    let c = crossover (sma n1 |> Frame.applyAdjClose) 
              (sma n2 |> Frame.applyAdjClose)
    let tg = buyAllSellAll security lotsize 
    strategy c nextBarOpen tg (cash initCapital) prices

  [<TestCase()>]
  let ``EMA``() =
    let v = [22.27; 22.19; 22.08; 22.17; 22.18; 22.13; 22.23; 22.43; 22.24; 
              22.29; 22.15; 22.39; 22.38; 22.61; 23.36; 24.05; 23.75; 23.83;
              23.95; 23.63; 23.82; 23.87; 23.65; 23.19; 23.10; 23.33; 22.68;
              23.10; 22.40; 22.17]
    let k = [for i in 0..(List.length v-1) -> 
                DateTime(2010, 3, 24) + TimeSpan(i*24, 0, 0)]
    let r = [22.22; 22.21; 22.24; 22.27; 22.33; 22.52; 22.80; 22.97; 23.13;
              23.28; 23.34; 23.43; 23.51; 23.54; 23.47; 23.40; 23.39; 23.26; 
              23.23; 23.08; 22.92]
    let e = v |> Series.ofValues |> Series.indexWith k |> ema 10
    printfn "test: %f expected: %f" (e |> Series.lastValue) (r |> List.last)
    e |> Series.lastValue |> should (equalWithin 0.01) (r |> List.last)
