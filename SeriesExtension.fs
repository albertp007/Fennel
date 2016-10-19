module Series

  open Deedle

  /// <summary>
  /// Skips rows in the beginning of a series while the value of the row meets
  /// with a certain condition specified by the function supplied
  /// </summary>
  /// <param name="f"></param>
  /// <param name="s"></param>
  let rec skipWhile f s =
    let k = Series.firstKey s
    let v = Series.firstValue s
    if f k v then Series.skip 1 s |> skipWhile f 
    else s





