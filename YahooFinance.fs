namespace QuantFin

module YahooFinance =

  open System
  open Deedle

  /// <summary>
  /// Type representing the fields of a candle bar
  /// </summary>
  type IBar =
    abstract Open: float
    abstract High: float
    abstract Low: float
    abstract Close: float
    abstract Volume: int64
    abstract AdjClose: float

  ///
  /// This is a type alias for the static CsvProvider type to use to retrieve
  /// csv output returned from Yahoo Finance historical prices url.
  /// CsvProvider depends on the FSharp.Data.dll assembly
  ///
  type Prices = FSharp.Data.CsvProvider<"Date(date), Open(float), \
  High(float), Low(float), Close(float), Volume(int64), AdjClose(float)">
  type Price = Prices.Row

  /// <summary>
  /// CsvProvider type for getting the prices of the current (or last) trading
  /// date.  The last price is used as both the close and adjusted close.  The
  /// only difference between this schema and the schema above is in HasHeaders
  /// </summary>
  type Quotes = FSharp.Data.CsvProvider<Schema="Date(date), Open(float), \
  High(float), Low(float), Close(float), Volume(int64), AdjClose(float)",
                                        HasHeaders=false>
  type Quote = Quotes.Row

  /// This is a private binding for a string print formatter to create the url
  /// to retrieve historical prices from Yahoo Finance from a certain start
  /// date
  ///
  let private urlFormatFromDate = sprintf "http://ichart.yahoo.com/table.csv\
  ?s=%s&a=%d&b=%d&c=%d&ignore=.csv"

  /// This is a private binding for a string print formatter to create the url
  /// to retrive historical prices from Yahoo Finance from the earliest date
  /// available
  ///
  let private urlFormat = sprintf "http://ichart.yahoo.com/table.csv?s=%s\
  &ignore=.csv"

  /// <summary>This function creates the URL for downloading historical prices
  /// from Yahoo Finance given the stock code (Reuters code) and the start
  /// date which is of format <c>yyyy-mm-dd</c></summary>
  /// <param name="stockCode">Reuters stock code of the stock</param>
  /// <param name="startDate">String in <c>yyyy-mm-dd</c> format representing
  /// the start date</param>
  /// <returns>the URL as a string</returns>
  ///
  let makeHistUrl stockCode (startDateOption: DateTime option) =
    match startDateOption with
    | None -> urlFormat stockCode
    | Some dt -> urlFormatFromDate stockCode (dt.Month - 1) dt.Day dt.Year

  /// <summary>
  /// Constructs the full path of the cache csv file given the stock code
  /// </summary>
  /// <param name="cachePath"></param>
  /// <param name="stockCode"></param>
  let makeCacheUrl cachePath stockCode =
    [|cachePath; sprintf "%s.csv" stockCode|]
    |> System.IO.Path.Combine
    |> System.IO.Path.GetFullPath

  /// <summary>
  /// Converts a sequence of CSV rows returned by the csv provider to a deedle
  /// data frame
  /// </summary>
  /// <param name="prices"></param>
  let csvToFrame (prices: Prices) =
    let f = prices.Rows
            |> Frame.ofRecords
            |> Frame.indexColsWith prices.Headers.Value
            |> Frame.indexRowsDate "Date"
            |> Frame.sortRowsByKey
            |> Frame.filterRows (fun _ r -> r?Volume > 0.0)
    // Remove spaces from all column keys
    f.ColumnKeys
    |> Seq.iter (
        fun k -> 
          if k.Contains(" ") then 
            f.RenameColumn(k, k.Replace(" ", "")))
    f

  /// <summary>This function downloads historical prices using a CsvProvider
  /// with schema representing the csv output from Yahoo Finance</summary>
  /// <param name="startDateOption">String in <c>yyyy-mm-dd</c> format
  /// representing the start date</param>
  /// <param name="stockCode">Reuters code of the stock</param>
  /// <returns>CsvProvider object with the date, open, high, low, close,
  /// volume and adjusted close prices in each row</returns>
  ///
  let downloadHistFromDate startDateOption stockCode =
    let url = makeHistUrl stockCode startDateOption
    Prices.Load url  
   
  /// <summary>This function simply curries downloadHistFromDate with the
  /// optional startDate set to None</summary>
  /// <param name="stockCode">Reuters code of the stock</param>
  /// <returns>CsvProvider object with the date, open, high, low, close,
  /// volume and adjusted close prices in each row</returns>
  ///
  let downloadHist = downloadHistFromDate None

  /// <summary>
  /// This function returns historical prices for a particular stock code from
  /// yahoo finance.  It first tries to hit the cache which is a directory
  /// containing csv files named according to the stock code.  If the file
  /// exists, it will read the file in, look up the latest date in the file and
  /// try to hit the yahoo finance website for any data that is subsequent to
  /// that date.  If any is found, it will combine the prices downloaded and
  /// then update the cache.  If nothing is found in the cache to begin with,
  /// it will simply download the prices from yahoo finance and then cache it
  /// before returning them in a deedle data frame
  /// </summary>
  /// <param name="cacheDir"></param>
  /// <param name="stockCode"></param>
  let hist cacheDir stockCode =
    let cachePath = stockCode |> makeCacheUrl cacheDir |> IO.Path.GetFullPath
    if cachePath |> System.IO.File.Exists then
      let cached = cachePath |> Prices.Load |> csvToFrame
      let latestDate = (cached.RowKeys |> Seq.max) + TimeSpan(1, 0, 0, 0)
      let missed =
        try downloadHistFromDate (Some latestDate) stockCode |> csvToFrame
        with
          | _ -> Frame.ofRows([])
      let combined = Frame.merge cached missed
      if not missed.IsEmpty then
        combined.SaveCsv(cachePath, true, ["Date"])
      combined
    else
      let prices = downloadHist stockCode
      if not (System.IO.Directory.Exists cacheDir) then
        System.IO.Directory.CreateDirectory cacheDir |> ignore
      prices.Save cachePath
      prices |> csvToFrame

  /// <summary>
  /// Retrieves the current or last trading date quote from yahoo finance. The
  /// last price is used as both the close and the adjusted close and constructs
  /// a deedle data frame which is compatible with the historical price data
  /// frame
  /// </summary>
  /// <param name="stockCode"></param>
  let quotes stockCodes =
    let rec makeStockString names =
      match names with
      | [] -> ""
      | [h] -> sprintf "%s" h
      | h::t -> sprintf "%s+%s" h (makeStockString t)

    let quoteUrl =
      stockCodes
      |> makeStockString
      |> sprintf "http://finance.yahoo.com/d/quotes.csv?f=d1ohgl1vl1&s=%s"

    let qs = Quotes.Load quoteUrl
    let f = qs.Rows |> Frame.ofRecords
    let columns = ["Date"; "Open"; "High"; "Low"; "Close"; "Volume"; "AdjClose"]   
    f.RenameColumns columns
    let qs = f |> Frame.addCol "Security" (stockCodes |> Seq.indexed |> series)
               |> Frame.indexRowsString "Security"
    qs?Close |> Series.observations |> Map.ofSeq

  /// <summary>
  /// Convert all rows of the data frame representing the prices coming back
  /// from Yahoo Finance into a series of IBar
  /// </summary>
  /// <param name="f"></param>
  let asBars (f: Frame<DateTime, string>) = f.GetRowsAs<IBar>()