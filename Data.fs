namespace QuantFin

module Data =
    ///
    /// This is a type alias for the static CsvProvider type to use to retrieve
    /// csv output returned from Yahoo Finance historical prices url.
    /// CsvProvider depends on the FSharp.Data.dll assembly
    ///
    type Prices = FSharp.Data.CsvProvider<"Date(date), Open(float), \
    High(float), Low(float), Close(float), Volume(int64), \"Adj Close\"(float)">

    type Bar = Prices.Row
