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
  let makeUrl stockCode (startDateOption: DateTime option) =
    match startDateOption with
    | None -> urlFormat stockCode
    | Some dt -> urlFormatFromDate stockCode (dt.Month - 1) dt.Day dt.Year

  /// <summary>This function downloads historical prices using a CsvProvider
  /// with schema representing the csv output from Yahoo Finance</summary>
  /// <param name="startDateOption">String in <c>yyyy-mm-dd</c> format
  /// representing the start date</param>
  /// <param name="stockCode">Reuters code of the stock</param>
  /// <returns>CsvProvider object with the date, open, high, low, close,
  /// volume and adjusted close prices in each row</returns>
  ///
  let downloadHistFromDate startDateOption stockCode =
    let url = makeUrl stockCode startDateOption
    let prices = Prices.Load url
    // prices.Rows |> Seq.map (fun r -> r.Date, r) |> Map.ofSeq
    let f = prices.Rows
            |> Frame.ofRecords
            |> Frame.indexColsWith prices.Headers.Value
            |> Frame.indexRowsDate "Date"
    // Remove spaces from all column keys
    f.ColumnKeys
    |> Seq.iter (fun k -> if k.Contains(" ") then 
                            f.RenameColumn(k, k.Replace(" ", "")))
    f
    
  /// <summary>This function simply curries downloadHistFromDate with the
  /// optional startDate set to None</summary>
  /// <param name="stockCode">Reuters code of the stock</param>
  /// <returns>CsvProvider object with the date, open, high, low, close,
  /// volume and adjusted close prices in each row</returns>
  ///
  let downloadHist = downloadHistFromDate None

  /// <summary>
  /// Convert all rows of the data frame representing the prices coming back
  /// from Yahoo Finance into a series of IBar
  /// </summary>
  /// <param name="f"></param>
  let asBars (f: Frame<DateTime, string>) = f.GetRowsAs<IBar>()