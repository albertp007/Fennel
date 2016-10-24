namespace QuantFin

module Util =

  open System

  let epsilon = 1.0e-8

  /// <summary>
  /// Round up a number to arbitrary number of precision
  /// </summary>
  /// <param name="x">Number to be rounded up</param>
  /// <param name="d">Rounding up to the nearest d</param>
  let roundUp x d =
    let tmp = x * (1.0/d)
    if abs(tmp - floor(tmp)) < epsilon then
      floor(tmp) * d
    else
      ceil(tmp) * d

  /// <summary>
  /// Round down a number to arbitrary number of precision
  /// </summary>
  /// <param name="x">Number to be rounded down</param>
  /// <param name="d">Rounding down to the nearest d</param>
  let roundDown x d =
    let tmp = x * (1.0/d)
    floor(tmp) * d

  /// <summary>
  /// Prepend a timestamp which is the current time in UTC to a given string
  /// </summary>
  let tsLog msg = 
    let ts = DateTime.UtcNow
    sprintf "%s - %s" (ts.ToString("dd/MMM/yyyy HH:mm:ss.fff")) msg
    |> printfn "%s"

  /// <summary>
  /// A simple function to split the input to and the result of a function into
  /// a pair
  /// </summary>
  /// <param name="f"></param>
  /// <param name="x"></param>
  let split f x = x, f x
    