namespace MathNet.Numerics.LinearAlgebra

module Vector =
  let inline ofSeq s = s |> Seq.toList |> vector
  let inline join v1 v2 =
    let v1List = v1 |> Vector.toList
    v2 |> Vector.toList |> List.append v1List |> vector

module Matrix =
  let inline aggCols f m = m |> Matrix.toColSeq |> Seq.map f |> Vector.ofSeq
  let inline aggRows f m = m |> Matrix.toRowSeq |> Seq.map f |> Vector.ofSeq
  let inline replicateRows n v = v|> Vector.toList |> List.replicate n |> matrix
  let inline replicateCols n v = replicateRows n v |> Matrix.transpose
  let inline ofRowVector v = v |> replicateRows 1
  let inline ofColVector v = v |> replicateCols 1
  let inline applyRowVector op v m = 
    v |> replicateRows (Matrix.rowCount m) |> op m
  let inline applyColVector op v m =
    v |> replicateCols (Matrix.columnCount m) |> op m
  

