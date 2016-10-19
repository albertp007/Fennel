module Frame

  open Deedle
  open System

  /// <summary>
  /// 
  /// </summary>
  /// <param name="n"></param>
  /// <param name="newKeyName"></param>
  /// <param name="frame"></param>
  /// <param name="keys"></param>
  let getNthRowAfterKeys n newKeyName (frame: Frame<'r, 'c>) keys =
    Seq.zip frame.RowKeys frame.RowKeys 
    |> series 
    |> Frame.addCol newKeyName <| frame
    |> Frame.shift -n 
    |> Frame.sliceRows keys
  