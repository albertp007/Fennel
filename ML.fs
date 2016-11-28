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

  let inline boolToFloat b = if b then 1.0 else 0.0
  let inline boolToInt b = if b then 1 else 0

  /// <summary>
  /// Function to normalize the set of features in the training set
  /// </summary>
  /// <param name="x"></param>
  let featureNormalize (x: Matrix<float>) =
    let (-) m v = Matrix.applyRowVector (-) v m
    let (./) m v = Matrix.applyRowVector (./) v m
    let mu = x |> Matrix.aggCols Statistics.Mean
    let sigma = x |> Matrix.aggCols Statistics.StandardDeviation
    mu, sigma, (x - mu) ./ sigma

  /// <summary>
  /// Function to evaluate the value of a linear regression hypothesis function
  /// and its partial derivates at a particular value of theta with regulation
  ///
  /// J = 1 / 2 / m * Sum ( (h(theta, X) - y) ^ 2 ) + lambda / 2 / m * 
  ///       ( theta1 ^ 2 + theta1 ^ 2 + ... + thetaN ^ 2)
  ///
  /// Note that theta0 is not included in the regulation term as theta0 is
  /// assumed to take on a constant value of 1.0.  Also, the first column of the
  /// matrix X is assumed to be the coefficients for theta0 and should be all
  /// ones
  ///
  /// The gradient function is:
  ///
  /// dJ/d(theta(i)) = 1 / m * sum ( (h(theta, X) - y) * x(i) ) if i = 0
  /// 
  /// dJ/d(theta(i)) = 1 / m * sum ( (h(theta, X) - y) * x(i) ) + 1/m * lambda *
  ///                   theta(i) if i > 0
  ///
  /// where the sums in the above two formulas is summing over all the entries
  /// in the training set (i.e. the number of rows in X) and the index i in the
  /// gradient function ranges over the number of features
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="th"></param>
  let lnrGrad (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (th: Vector<float>) =

    let m = float x.RowCount
    let h = x * th
    let thReg = th.Clone()
    thReg.[0] <- 0.0  // assume the first element is th0 and do not regulate
    let j0 = 0.5/m* (((x*th - y) .^ 2.0) |> Vector.sum)
    let j = j0 + lambda / 2.0 / m * ((thReg .^ 2.0) |> Vector.sum)
    let dj = 1.0 / m * x.Transpose() * (h - y) + lambda / m * thReg
    dj, j

  /// <summary>
  /// Function to evaluate the value of a logistic regression hypothesis function
  /// and its partial derivates at a particular value of theta with regulation
  ///
  /// J = 1/m * Sum (-y*log(sigmoid(X*theta))-(1-y)*(1-log(sigmoid(X*theta)))) +
  ///     lambda/2/m*(theta1^2 + ... + thetaN^2)
  ///
  /// Note that theta0 is not included in the regulation term as theta0 is
  /// assumed to take on a constant value of 1.0.  Also, the first column of the
  /// matrix X is assumed to be the coefficients for theta0 and should be all
  /// ones
  ///
  /// The gradient function is:
  ///
  /// dJ/d(theta(i)) = 1 / m * sum ( (h(theta, X) - y) * x(i) ) if i = 0
  /// 
  /// dJ/d(theta(i)) = 1 / m * sum ( (h(theta, X) - y) * x(i) ) + 1/m * lambda *
  ///                   theta(i) if i > 0
  ///
  /// where the sums in the above two formulas is summing over all the entries
  /// in the training set (i.e. the number of rows in X) and the index i in the
  /// gradient function ranges over the number of features
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="th"></param>
  let lgrGrad (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (th: Vector<float>) =
  
    let m = float x.RowCount
    let h = Vector.sigmoid (x*th)
    let thReg = th.Clone()
    thReg.[0] <- 0.0  // assume the first element is th0 and do not regulate
    let j0 = 1.0/m * ((-y) * (log h) - (1.0 - y)*(log (1.0 - h)))
    let j = j0 + lambda / 2.0 / m * ((thReg .^ 2.0) |> Vector.sum)
    let dj = 1.0 / m * x.Transpose() * (h - y) + lambda / m * thReg
    dj, j

  /// <summary>
  /// The update function for updating theta when using gradient descent for
  /// logistic regression
  /// </summary>
  /// <param name="alpha"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let inline lgrTheta (lambda: float) (alpha: float) (x: Matrix<float>) 
    (y: Vector<float>) t = 

    let m = float x.RowCount
    t - alpha / m * (x.Transpose()) * (Vector.sigmoid (x*t) - y)
 
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

  /// <summary>
  /// Gradient descent search algorithm given the number of iterations, the
  /// alpha, a function returning a vector of partial derivatives and the
  /// value evaulated at a particular point and an initial value to start with
  /// </summary>
  /// <param name="n"></param>
  /// <param name="alpha"></param>
  /// <param name="gradFunc"></param>
  /// <param name="init"></param>
  let gradientDescent n (alpha: float) 
    (gradFunc:Vector<float>->Vector<float>*float) (init: Vector<float>) =
    let th = init.Clone()
    let f (t, c) =
      let grad, j = gradFunc t
      Vector.subInPlace t (alpha * grad)
      t, j::c
    let (t, c) = [1..n] |> List.fold (fun s _ -> f s) (th, [])
    t, List.rev c    