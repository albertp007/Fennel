namespace QuantFin

module Signal =

  type TechnicalIndicator =
    | Rsi of int
    | Sma of int
    | Ema of int
    | Macd of int * int * int
    | Open
    | Close
    | Volume

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

  /// <summary>Calculates n day RSI given a list of bars</summary>
  /// <param name="n">number of days</param>
  /// <param name="prices">list of QuantFin.Data.Bars</param>
  /// <returns>list of RSI with the first n entries being zero</returns>
  ///
  let rsi n (prices: QuantFin.Data.Bar list) =
    let ma = movingAverage n
    prices
     // extract close prices from list of bars
     |> List.map (fun x->x.c)
     // calculate differences between adjacent terms
     |> filter [-1.0; 1.0]
     |> Seq.toList
     // create list of pair (u, d)
     |> List.map (fun x->if x < 0.0 then (0.0, -x) else (x, 0.0))
     // combine into list of u and list of d
     |> List.unzip
     // calculate moving average of list of u and list of d
     |> fun (u,d)->(ma u, ma d)
     |> fun (mau, mad)->List.zip mau mad
     |> List.map (fun (mau, mad) ->
        let rs = if mad <= 0.0 then 0.0 else mau/mad
        100.0 - 100.0/(1.0 + rs))

  /// <summary> Calculates simple moving average of the close price given
  /// a list of bars </summary>
  /// <param name="n">Size of the moving window</param>
  /// <param name="prices">list of bars</param>
  /// <returns>list of floats</returns>
  ///
  let sma n (prices: QuantFin.Data.Bar list) =
    let ma = movingAverage n
    prices |> List.map (fun x->x.c) |> ma |> Seq.toList

  /// <summary>Calculates different types of technical indicators given a list
  /// of bars </summary>
  /// <param name="t">the technical indicator of type TechnicalIndicator</param>
  /// <param name="prices">the list of price bars</param>
  /// <returns>list of floats</returns>
  ///
  let calcTI t (prices: QuantFin.Data.Bar list) =
    match t with
    | Rsi n -> rsi n prices
    | Sma n -> sma n prices
    | Ema n -> sma n prices          // TODO placeholder for now
    | Macd (a, _, _) -> sma a prices // TODO placeholder for now
    | Open -> prices |> List.map (fun b->b.o)
    | Close -> prices |> List.map (fun b->b.c)
    | Volume -> prices |> List.map (fun b->float b.v)

  /// <summary>Augments the list of price bars with one technical indicator
  /// </summary>
  /// <param name="ti">the technical indicator of type TechnicalIndicator
  /// </param>
  /// <param name="prices">the list of price bars</param>
  /// <returns>list of tuple of price bar and float which is the value of the
  /// technical indicator calculated</returns>
  ///
  let augment ti prices =
    calcTI ti prices |> List.zip prices

  /// <summary>Type representing the interface of a signal generator expected
  /// by the findPattern function</summary>
  /// <param name="augment">the augment function to augment the list of price
  /// bars expected by the function defined by the find function</param>
  /// <param name="find">the function used to check whether the list of price
  /// bars augmented by the augment function contains a pattern or signal at
  /// the head of the list</param>
  ///
  type ISignalGenerator<'A, 'S> =
    abstract augment : QuantFin.Data.Bar list -> 'A list
    abstract find : 'A list -> int -> 'S option

  /// <summary>The find function defines the bearish engulfing candlestick
  /// pattern.  The augment function augments the list of prices by RSI
  /// </summary>
  /// <param name="pList">list of price bars to process</param>
  /// <param name="augList">list of augmented price bars</param>
  /// <returns>optional pair of System.DateTime indicating the start time and
  /// end time of the pattern if found</returns>
  ///
  let bearishEngulf = {
    new ISignalGenerator<_,_> with
      member this.augment pList = augment (Rsi 14) pList
      member this.find augList n =
        match augList with
        | (h1, rsi1)::(h2, rsi2)::_ ->
            if h2.o > h1.c && h2.c < h1.o &&
              h1.o < h1.c && rsi1 > 80.0 then
              Some (h1.d, h2.d, rsi1, rsi2, n+2) else None
        | _ -> None
  }

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

  let maFast n l =
    let q0 = QuantFin.Queue.makeQueue n
    let f (q, a) x =
      match (QuantFin.Queue.push q x) with
      | (q', Some x') ->
          let a' = (a*(float n)-x'+x)/(float n)
          (q', a'), a'
      | (q', None) ->
          if QuantFin.Queue.size q' = QuantFin.Queue.capacity q' then
            let a' = QuantFin.Queue.toSeq q' |> Seq.average
            ((q', a'), a')
          else
            ((q', 0.0), 0.0)
    QuantFin.Data.foldState f (q0, 0.0) [] l
