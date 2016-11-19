namespace QuantFin

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics

module ML =

  /// <summary>
  /// Generic implementation of the K-Nearest Neighbour algorithm
  /// </summary>
  /// <param name="distance">Function that calculates the distance between
  /// two data points of generic type</param>
  /// <param name="k"></param>
  /// <param name="training"></param>
  /// <param name="newPoint"></param>
  let kNN (distance:'p->'p->float) k training newPoint =
    training
    |> Seq.map (fun (tag, point) -> tag, distance point newPoint)
    |> Seq.sortBy snd
    |> Seq.take k
    |> Seq.countBy fst

  /// <summary>
  /// Function to normalize the set of features in the training set
  /// </summary>
  /// <param name="x"></param>
  let featureNormalize (x: Matrix<float>) =
    let repl v = Matrix.replicateRows x.RowCount v
    let mu = x |> Matrix.aggCols Statistics.Mean
    let sigma = x |> Matrix.aggCols Statistics.StandardDeviation
    let mu', sigma' = mu |> repl, sigma |> repl
    mu, sigma, (x - mu') ./ sigma'

  /// <summary>
  /// The update function for updating theta when using gradient descent for
  /// linear regression
  /// </summary>
  /// <param name="alpha"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let inline lnrTheta (alpha: float) (x: Matrix<float>) (y: Vector<float>) t =
    let m = float x.RowCount
    let (z: Vector<float>) = ( x * t - y)
    t - alpha / m * (x.Transpose()) * z

  /// <summary>
  /// The update function for updating theta when using gradient descent for
  /// logistic regression
  /// </summary>
  /// <param name="alpha"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let inline lgrTheta (alpha: float) (x: Matrix<float>) (y: Vector<float>) t = 
    let m = float x.RowCount
    let sigmoid (x:Vector<float>) = 1.0 / (1.0 + (-x).PointwiseExp())
    t - alpha / m * (x.Transpose()) * (sigmoid(x*t) - y)
  
  /// <summary>
  /// Apply a function on a monoid N number of times
  /// </summary>
  /// <param name="f"></param>
  /// <param name="n"></param>
  /// <param name="init"></param>
  let apply f n init =
    [1..n] |> List.fold (fun s i -> f s) init

  /// <summary>
  /// Apply a function on a monoid N number of times, collecting intermediate
  /// results in a list
  /// </summary>
  let applyScan f n init =
    [1..n] |> List.scan (fun s i -> f s) init
