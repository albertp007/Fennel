namespace QuantFin

module YahooFinance =

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
    let makeUrl stockCode startDateOption =
        match startDateOption with
        | None -> urlFormat stockCode
        | Some startDate -> let dt = System.DateTime.Parse startDate
                            urlFormatFromDate stockCode (dt.Month - 1) dt.Day
                                dt.Year

    /// <summary>This function downloads historical prices using a CsvProvider
    /// with schema representing the csv output from Yahoo Finance</summary>
    /// <param name="startDateOption">String in <c>yyyy-mm-dd</c> format
    /// representing the start date</param>
    /// <param name="stockCode">Reuters code of the stock</param>
    /// <returns>CsvProvider object with the date, open, high, low, close,
    /// volume and adjusted close prices in each row</returns>
    ///
    let downloadHistFromDate startDateOption stockCode  =
        let url = makeUrl stockCode startDateOption
        QuantFin.Data.Prices.Load url

    /// <summary>This function simply curries downloadHistFromDate with the
    /// optional startDate set to None</summary>
    /// <param name="stockCode">Reuters code of the stock</param>
    /// <returns>CsvProvider object with the date, open, high, low, close,
    /// volume and adjusted close prices in each row</returns>
    ///
    let downloadHist = downloadHistFromDate None
