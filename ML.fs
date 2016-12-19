namespace QuantFin

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open DotNumerics.Optimization

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
  /// Private helper function to evaluate the value of the regulated logistic 
  /// regression cost function at a particular point.  It assumes the vector 
  /// h(theta, x) is passed in.  This is such that in an implementation where
  /// the cost and the gradient are to be returned at the same time, h(theta, x)
  /// is calculated once and shared between evaluating the cost and the partial
  /// derivatives
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="h"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let private lgrCost0 (lambda: float) (h: Vector<float>) (y: Vector<float>) 
      (theta: Vector<float>) =
    // h is assumed to be sigmoid(x*theta), which is a vector
    let m = float h.Count
    let thReg = theta.Clone()
    thReg.[0] <- 0.0  // assume the first element is th0 and do not regulate
    let j0 = 1.0/m * ((-y) * (log h) - (1.0 - y)*(log (1.0 - h)))
    j0 + lambda / 2.0 / m * ((thReg .^ 2.0) |> Vector.sum)

  /// <summary>
  /// Private helper function to evaluate the partial derivatives of the
  /// regulated logistic regression cost function at a particular point.  It
  /// assumes the vector h(theta, x) is passed in.  This is such that in an
  /// implementation where the cost and the gradient are to be returned at the
  /// same time, h(theta, x) is calculated once and shared between evaluating
  /// the cost and the partial derivatives
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  /// <param name="h"></param>
  let private lgrGrad0 (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (theta: Vector<float>) (h: Vector<float>) =
    // h is assumed to be sigmoid(x*theta), which is a vector
    let m = float x.RowCount
    let thReg = theta.Clone()
    thReg.[0] <- 0.0  // assume the first element is th0 and do not regulate
    1.0 / m * x.Transpose() * (h - y) + lambda / m * thReg

  /// <summary>
  /// Function to evaluate the value of the regulated cost function for logistic 
  /// regression
  ///
  /// J = 1/m * Sum (-y*log(sigmoid(X*theta))-(1-y)*(1-log(sigmoid(X*theta)))) +
  ///     lambda/2/m*(theta1^2 + ... + thetaN^2)
  ///
  /// Note that theta0 is not included in the regulation term as theta0 is
  /// assumed to take on a constant value of 1.0.  Also, the first column of the
  /// matrix X is assumed to be the coefficients for theta0 and should be all
  /// ones.
  ///
  /// The actual implementation is in the helper function lgrCost0.
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let lgrCost (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (theta: Vector<float>) =
    let h = Vector.sigmoid(x*theta)
    lgrCost0 lambda h y theta

  /// <summary>
  /// Function that evaluates the partial derivates of the regulated cost 
  /// function for logistic regression
  ///
  /// dJ/d(theta(i)) = 1 / m * sum ( (h(theta, X) - y) * x(i) ) if i = 0
  /// 
  /// dJ/d(theta(i)) = 1 / m * sum ( (h(theta, X) - y) * x(i) ) + 1/m * lambda *
  ///                   theta(i) if i > 0
  ///
  /// Note that theta0 is not included in the regulation term as it is assumed
  /// to take on a constant value of 1.0.    Also, the first column of the
  /// matrix X is assumed to be the coefficients for theta0 and should be all
  /// ones.
  ///
  /// The actual implementation is in the helper function lgrGrad0.
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let lgrGrad (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (theta: Vector<float>) =  
    let h = Vector.sigmoid(x*theta)
    lgrGrad0 lambda x y theta h

  /// <summary>
  /// Function to evaluate the value and the partial derivatives of the 
  /// regulated cost function for logistic regression.  It simply calls
  /// lgrCost and lgrGrad and returns a pair
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="th"></param>
  let lgrGradCost (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (th: Vector<float>) = 
    let h = Vector.sigmoid (x*th)
    lgrGrad0 lambda x y th h, lgrCost0 lambda h y th

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
    
  /// <summary>
  /// Converts a real-valued multi-variable function from vector form to float
  /// array form
  /// </summary>
  /// <param name="f"></param>
  let inline toArrayFunc (f: Vector<float>->float) = vector >> f

  /// <summary>
  /// Converts a gradient function from vector from to float array form
  /// </summary>
  /// <param name="f"></param>
  let inline toArrayGradFunc (f: Vector<float>->Vector<float>) =
    vector >> f >> Vector.toArray

  /// <summary>
  /// Calls the BFGS optimizer in DotNet Numerics with vectors in Mathnet 
  /// Numerics.  The BFGS optimizer takes double arrays in its arguments. This
  /// function is a helper function which converts the input from the vector
  /// form to double arrays and converts the output of the optimizer from
  /// double array back to vector.
  /// </summary>
  /// <param name="f">the function to minimize</param>
  /// <param name="g">the gradient of the function</param>
  /// <param name="initial">initial guess</param>
  let bfgs (f:Vector<float>->float) (g: Vector<float>->Vector<float>) initial =
     let f' = f |> toArrayFunc
     let g' = g |> toArrayGradFunc
     let optimizer = L_BFGS_B()
     optimizer.ComputeMin( f', g', initial |> Vector.toArray )
     |> vector

  let swap i j (arr: 'a[]) =
    let tmp = arr.[i]
    arr.[i] <- arr.[j]
    arr.[j] <- tmp
    
  let knuthShuffle (s: seq<'a>) =
    let arr = Seq.toArray s
    let size = arr |> Array.length
    for i in (size-1)..(-1)..1 do
      let rnd = DiscreteUniform.Sample(0, i-1)
      swap rnd i arr
    arr |> Array.toSeq
    
  /// <summary>
  /// Generate the dimensions of the theta matrices given a list of integers
  /// representing the number of nodes in each layer, not counting in the bias
  /// node.  Element 0 should be the number of input features and the last
  /// element should be the number of output nodes
  /// </summary>
  /// <param name="layers"></param>
  let makeThetaDim layers =
    layers
    |> List.rev
    |> List.windowed 2
    |> List.map (
        fun e -> 
          match e with
          | [a; b] -> (a, b+1)
          | _ -> failwith "Invalid case")
    |> List.rev

  /// <summary>
  /// Forward propagates from the layer of nodes where the given activations are
  /// </summary>
  /// <param name="activations">Activation matrix in this layer. Each row
  /// represents one training example and each value in the row is the value
  /// of each nodes in that layer</param>
  /// <param name="theta">Theta matrix.  The number of rows is equal to the
  /// number of nodes in the next layer and the number of columns is the number
  /// of nodes in the current layer + 1 which is the bias node</param>
  let forwardPropagate activations (theta: Matrix<'a>) =
    activations 
    |> Matrix.prependColumnOnes 
    |> fun m -> m * theta.Transpose() 
    |> Matrix.sigmoid

  /// <summary>
  /// Back-propagation algorithm to calculate the deltas from the delta in the
  /// l+1 layer, the theta and the activation matrices of the l-layer
  /// </summary>
  /// <param name="delta">Delta in the l+1 layer</param>
  /// <param name="theta">Theta in the current layer</param>
  /// <param name="activation">Activations in the current layer</param>
  let backPropagate (delta: Matrix<float>) (theta, activation) =
    let activation1 = activation |> Matrix.prependColumnOnes
    let delta' = (delta * theta) .* activation1 .* (1.0 - activation1)
    delta'.[0.., 1..]

  /// <summary>
  /// Cost function for neural network
  /// </summary>
  /// <param name="x">The feature matrix, where the number of rows is the
  /// the number of examples and the number of columns is the number of features
  /// in each example</param>
  /// <param name="y">The output matrix, where the number of rows is the number
  /// of examples and the number of columns is the number of nodes in the final
  /// layer</param>
  /// <param name="layers">Sequence of integers specifying the number of nodes
  /// in each layer.  Element 0 is thus the number of input features and the
  /// last element is the number of nodes in the output layer</param>
  /// <param name="lambda">Regularization parameter</param>
  /// <param name="thetaArray">The theta matrices of all layers unrolled into
  /// a single array, column-major-wise</param>
  let nnCost x y layers lambda thetaArray =
    let dims = layers |> makeThetaDim
    // Could have used x below but using y instead so that its type can be
    // automatically inferred
    let m = y |> Matrix.rowCount
    let thetas = thetaArray |> Matrix.reshape dims
    let h = thetas |> Seq.fold forwardPropagate x
    let reg = 
      thetas
      // Exclude bias nodes in regularization, hence the slice
      |> Seq.fold (fun s m -> s + (m.[0.., 1..] .^ 2.0 |> Matrix.sum)) 0.0
      |> (*) (lambda / 2.0 / float m)
    ((-y .* log h) - (1.0-y) .* log (1.0 - h) |> Matrix.sum) / (float m) + reg

  /// <summary>
  /// Gradient function for neural network using the back propagation algorithm
  /// </summary>
  /// <param name="x">The feature matrix of all training examples</param>
  /// <param name="y">The result of all training examples</param>
  /// <param name="layers">List of integers representing the number of nodes
  /// excluding the bias node in each layer of the network</param>
  /// <param name="lambda">Regularization parameter</param>
  /// <param name="thetaArray">Theta matrices of all layers unrolled into an
  /// array, column-major-wise.  See Matrix.unroll and Matrix.reshape</param>
  let nnGrad x y layers lambda thetaArray =    
    let dims = layers |> makeThetaDim
    // Could have used x below but using y instead so that its type can be
    // automatically inferred
    let m = y |> Matrix.rowCount
    let thetas = thetaArray |> Matrix.reshape dims
    let activations = thetas |> Seq.scan forwardPropagate x
    let output = activations |> Seq.last
    let deltaLast = output - y
    let deltas = 
      activations
      |> Seq.take (Seq.length activations - 1)
      |> Seq.tail // we don't need the first activation which is simply the input
      |> Seq.rev
      |> Seq.zip (thetas |> Seq.rev)
      |> Seq.scan backPropagate deltaLast
      |> Seq.rev
    let thetaRegs =
      thetas
      |> Seq.map (
        fun theta -> 
          theta.[0.., 1..] 
          |> Matrix.prependCol (DenseVector.zero<float> theta.RowCount )
          |> (*) (lambda/(float m )))
    deltas
    |> Seq.zip activations  // last element of activations will be ignored
    |> Seq.map (fun (activation, delta) -> 
         delta.Transpose() * (activation |> Matrix.prependColumnOnes) / float m)
    |> Seq.map2 (+) thetaRegs
    |> Matrix.unroll

  /// <summary>
  /// Calculates the gradient of a multi-dimensional function by perturbation
    /// </summary>
  /// <param name="f">the function which takes an array of floats and returns
  /// a float</param>
  /// <param name="thetas"></param>
  let computeNumericalGradient (f: (float[]->float)) (thetas: float[]) =
    let e = 1e-4;
    let perturb = Array.create (thetas.Length) 0.0
    let result = Array.create (thetas.Length) 0.0
    for i in 0..(thetas.Length-1) do
      if i > 0 then perturb.[i-1] <- 0.0
      perturb.[i] <- e
      let thetasV = thetas |> vector
      let perturbV = perturb |> vector
      let plus = (thetasV + perturbV) |> Vector.toArray
      let minus = (thetasV - perturbV) |> Vector.toArray
      result.[i] <- (f plus - f minus) / 2.0 / e
    result

module UnitTests =

  open NUnit.Framework
  open FsUnit
  open MathNet.Numerics.LinearAlgebra
  open ML

  [<TestCase("3, 5, 3", 5, 0.0)>]
  [<TestCase("30, 45, 10", 5, 0.0)>]
  [<TestCase("3, 10, 5, 3", 5, 0.0)>]
  [<TestCase("3, 25, 10, 5, 3", 5, 0.0)>]
  let ``NN Gradient`` (layerString: string, m, lambda) =

    let layers = 
      layerString.Split([|','|]) 
      |> Array.map (System.Int32.Parse) 
      |> Array.toList
    let nFeatures = layers.[0]
    let nLabels = layers |> List.last

    let initTheta (r, c) =
      [1.0..(float)(r*c)] 
      |> vector 
      |> sin 
      |> Matrix.reshapeVector [(r, c)]
      |> Seq.head

    let dims = layers |> makeThetaDim
    let thetas = dims |> Seq.map initTheta
    let x = initTheta (m, nFeatures)
    let y =
      let a = CreateMatrix.Dense(m, nLabels)
      [|1.0..(float m)|]
      |> Matrix.reshape [(m, 1)]
      |> Seq.head
      |> fun u -> u % (float nLabels) + 1.0
      |> Matrix.iteri (fun r c v -> a.[r, (int v)-1] <- 1.0)
      a

    let costFunc = nnCost x y layers lambda
    let numGrad = 
      thetas |> Matrix.unroll |> computeNumericalGradient costFunc |> vector
    let grad = thetas |> Matrix.unroll |> nnGrad x y layers lambda |> vector
    (numGrad - grad)
    |> Vector.sum
    |> should be (lessThan 1e-04)




      



