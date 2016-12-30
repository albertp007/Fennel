namespace QuantFin

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open MathNet.Numerics
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
  /// Normalize the given matrix X, where the number of rows is the number of
  /// examples and the number of columns is the number of features, given
  /// vectors of mu and sigma, representing the mean and standard deviation of
  /// each feature
  /// </summary>
  /// <param name="mu">Vector representing the mean of each feature</param>
  /// <param name="sigma">Vector representing the standard deviation of each
  /// factor</param>
  /// <param name="x">Matrix representing the training examples where the number
  /// of rows is the number of examples and the number of columns is the number
  /// of features</param>
  let normalize (mu: Vector<float>) (sigma: Vector<float>) (x: Matrix<float>) =
    let (-) m v = Matrix.applyRowVector (-) v m
    let (./) m v = Matrix.applyRowVector (Matrix.map2 (fun v1 v2 -> if v2 = 0.0 then v1 else v1/v2)) v m
    (x - mu) ./ sigma

  /// <summary>
  /// Calculates the mean and standard deviation of each feature in the training
  /// example matrix where the number of rows is the number of examples and the
  /// number of columns is the number of features, and then normalize the
  /// matrix, returning a triplet of mean and standard deviation of each feature
  /// as vectors and the normalized example matrix
  /// </summary>
  /// <param name="x"></param>
  let featureNormalize (x: Matrix<float>) =
    let mu = x |> Matrix.aggCols Statistics.Mean
    let sigma = x |> Matrix.aggCols Statistics.StandardDeviation
    mu, sigma, normalize mu sigma x

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
  let linearGrad (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
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
  let private logisticCost0 (lambda: float) (h: Vector<float>) (y:Vector<float>) 
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
  let private logisticGrad0 (lambda: float) (x: Matrix<float>) (y:Vector<float>) 
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
  /// The actual implementation is in the helper function logisticCost0.
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let logisticCost (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (theta: Vector<float>) =
    let h = Vector.sigmoid(x*theta)
    logisticCost0 lambda h y theta

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
  /// The actual implementation is in the helper function logisticGrad0.
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="theta"></param>
  let logisticGrad (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (theta: Vector<float>) =  
    let h = Vector.sigmoid(x*theta)
    logisticGrad0 lambda x y theta h

  /// <summary>
  /// Function to evaluate the value and the partial derivatives of the 
  /// regulated cost function for logistic regression.  It simply calls
  /// logisticCost and logisticGrad and returns a pair
  /// </summary>
  /// <param name="lambda"></param>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="th"></param>
  let logisticGradCost (lambda: float) (x: Matrix<float>) (y: Vector<float>) 
      (th: Vector<float>) = 
    let h = Vector.sigmoid (x*th)
    logisticGrad0 lambda x y th h, logisticCost0 lambda h y th

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
  /// Calls the BFGS optimizer in DotNet Numerics with arrays in Mathnet 
  /// Numerics.  The BFGS optimizer takes double arrays in its arguments. This
  /// function is a helper function which converts the input from the vector
  /// form to double arrays and converts the output of the optimizer from
  /// double array back to vector.
  /// </summary>
  /// <param name="tolerance">Tolerance param of the L_BFGS_B optimizer</param>
  /// <param name="f">the function to minimize</param>
  /// <param name="g">the gradient of the function</param>
  /// <param name="initial">initial guess</param>
  let bfgs tolerance (f: float[]->float, g: float[]->float[], initial: float[]) =
              
    let optimizer = L_BFGS_B()
    optimizer.Tolerance <- tolerance
    optimizer.ComputeMin( f, g, initial )

  /// <summary>
  /// This is similar to bfgs except that it takes functions which have
  /// arguments in a Mathnet Numerics vector rather than an array
  /// </summary>
  /// <param name="tolerance"></param>
  /// <param name="f"></param>
  /// <param name="g"></param>
  /// <param name="init"></param>
  let bfgsVec tolerance (f:Vector<float>->float) 
    (g: Vector<float>->Vector<float>) init =

     let f' = f |> toArrayFunc
     let g' = g |> toArrayGradFunc
     bfgs tolerance (f', g', init |> Vector.toArray ) |> vector

  /// <summary>
  /// A version of bfgs optimizer that uses one function to return both cost
  /// and gradient vector instead of separate functions.  This might allow
  /// slightly more efficient implementation in some cases where some code and
  /// values can be shared between the calculation of the cost and the gradient
  /// </summary>
  /// <param name="tolerance"></param>
  /// <param name="f"></param>
  /// <param name="initial"></param>
  let bfgs1 tolerance (f: float[]->(float*float[]), initial: float[]) =
              
    let f1 = new System.Func<float[], float*float[]>( f )
    let optimizer = L_BFGS_B()
    optimizer.Tolerance <- tolerance
    optimizer.ComputeMin( f1, initial )

  /// <summary>
  /// Swap element with index i and j in an array in place
  /// </summary>
  /// <param name="i"></param>
  /// <param name="j"></param>
  /// <param name="arr"></param>
  let swap i j (arr: 'a[]) =
    let tmp = arr.[i]
    arr.[i] <- arr.[j]
    arr.[j] <- tmp
    
  /// <summary>
  /// Generates a random permutation of a given sequence of elements
  /// </summary>
  /// <param name="s"></param>
  let knuthShuffle (s: seq<'a>) =
    let arr = Seq.toArray s
    let size = arr |> Array.length
    for i in (size-1)..(-1)..1 do
      let rnd = DiscreteUniform.Sample(0, i-1)
      swap rnd i arr
    arr

  /// <summary>
  /// Randomly permute the rows of a matrix
  /// </summary>
  /// <param name="m"></param>
  let randomPermuteMatrixRows (m: Matrix<'a>) =
    let clone = m.Clone();
    let p = Permutation(knuthShuffle [0..(m.RowCount-1)])
    clone.PermuteRows(p)
    clone

  /// <summary>
  /// Splits the data set matrix sequentially into a training set, a test data
  /// set and optionally a cross validation data set according to the 
  /// percentages of training and test examples.  The percentages must NOT add
  /// up to be larger than 1.0 otherwise an exception will be thrown.  The
  /// cross validation set is whatever is left over of the training and test
  /// data sets and therefore is returned as a matrix option
  /// </summary>
  /// <param name="trainingPerc"></param>
  /// <param name="testPerc"></param>
  /// <param name="data"></param>
  let splitDataSet trainingPerc testPerc (data: Matrix<'a>) =
    let trainingPerc', testPerc' = abs trainingPerc, abs testPerc
    if trainingPerc' + testPerc' > 1.0 then 
      failwith "Percentages add up to be larger than 1.0"
    let cvPerc' = 1.0 - trainingPerc' - testPerc'
    let m = data.RowCount
    let numTraining = (float m) * trainingPerc' |> round |> int
    let numTest = (float m) * testPerc' |> round |> int
    let numCV = m - numTraining - numTest
    (data.[0..(numTraining-1), 0..],
     (if numTest > 0 then Some data.[numTraining..(numTraining+numTest-1), 0..]
      else None),
     (if numCV > 0 then Some data.[numTraining+numTest.., 0..] else None))

  /// <summary>
  /// Splits a data set matrix into the feature matrix and the actual output
  /// using the last column as the actual output
  /// </summary>
  /// <param name="data"></param>
  let useLastColAsY (data: Matrix<'a>) =
    (data.[0.., 0..(data.ColumnCount-2)],
     data.[0.., (data.ColumnCount-1)])
   
  /// <summary>
  /// Calculates the number of nodes in each layers given the number of nodes
  /// in each hidden layers, the input and the output.  The number of nodes in 
  /// the first layer is calculated based on the number of features in the input
  /// which is assumed to be a matrix of m rows times n columns where m is the
  /// number of training examples and n is the number of features.  The number 
  /// of nodes in the last layer is simply the number of output nodes and is
  /// calculated based on the matrix y, where the number of rows in y is the
  /// number of training examples and the number of columns is the number of
  /// output nodes.  The number of nodes in each hidden layers, excluding the
  /// bias node, is given in a sequence of integers</summary>
  /// <param name="x">The input matrix where the number of rows is the number
  /// of training examples and the number of columns is the number of features
  /// </param>
  /// <param name="y">The output matrix where the number of rows is the number
  /// of training examples and the number of columns is the number of output
  /// nodes</param>
  /// <param name="hiddenLayerSeq">A sequence containing the number of nodes
  /// in each hidden layers.  The length of this sequence is exactly the number
  /// of hidden layers</param>
  let makeLayerSeq x y hiddenLayerSeq =
    [[x |> Matrix.columnCount]; hiddenLayerSeq; [y |> Matrix.columnCount]] 
    |> Seq.concat

  /// <summary>
  /// Generate the dimensions of the theta matrices given a list of integers
  /// representing the number of nodes in each layer, not counting in the bias
  /// node.  Element 0 should be the number of input features and the last
  /// element should be the number of output nodes
  /// </summary>
  /// <param name="layers"></param>
  let makeThetaDim layers =
    layers
    |> Seq.toList
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
  let forwardPropagate activations (theta: Matrix<float>) =
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
  /// <param name="hiddenLayers">Sequence of integers specifying the number of
  /// nodes in each hidden layer, not including the input layer and the output
  /// layer as these can be derived using the input and output matrixand also
  /// excluding the bias node in each hidden layer of the network</param>
  /// <param name="lambda">Regularization parameter</param>
  /// <param name="thetaArray">The theta matrices of all layers unrolled into
  /// a single array, column-major-wise</param>
  let nnCost x y hiddenLayers lambda thetaArray =
    let dims = hiddenLayers |> makeLayerSeq x y |> makeThetaDim
               
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
  /// Helper function to calculate the gradient of the neural network cost
  /// function.  This takes care of the more complicated part of the calculation
  /// which includes running backward propagations to get the deltas of each
  /// hidden layer (i.e. excluding the input layer and the output layer) and
  /// then going forward once again to multiply the activations of each layer
  /// by the delta of each layer.  Finally, add on the regularization term.
  /// </summary>
  /// <param name="m"></param>
  /// <param name="lambda"></param>
  /// <param name="deltaLast"></param>
  /// <param name="activations"></param>
  /// <param name="thetas"></param>
  let private nnGrad0 m lambda deltaLast activations (thetas: seq<Matrix<float>>) =
    let deltas = 
      activations // this is in the forward direction
      // we don't need the last activation which is simply the values of the
      // output nodes and that's encapsulated in deltaLast, hence take one less
      |> Seq.take (Seq.length activations - 1)
      |> Seq.tail // we don't need the first one which is simply the input
      |> Seq.rev  // doing it backwards - backward propagation
      // we need Theta(l), Activation(l) and delta(l+1) to get delta(l), hence
      // the zip
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
    |> Seq.map2 (fun activation delta -> 
         delta.Transpose() * (activation |> Matrix.prependColumnOnes) / float m)
         activations
    |> Seq.map2 (+) thetaRegs
    |> Matrix.unroll

  /// <summary>
  /// Gradient function for neural network using the back propagation algorithm
  /// </summary>
  /// <param name="x">The feature matrix of all training examples</param>
  /// <param name="y">The result of all training examples</param>
  /// <param name="hiddenLayers">Sequence of integers representing the number of
  /// nodes in each hidden layer, not including the input layer and the output
  /// layer as these can be derived using the input and output matrix and also
  /// excluding the bias node in each hidden layer of the network</param>
  /// <param name="lambda">Regularization parameter</param>
  /// <param name="thetaArray">Theta matrices of all layers unrolled into an
  /// array, column-major-wise.  See Matrix.unroll and Matrix.reshape</param>
  let nnGrad x y hiddenLayers lambda thetaArray =    
    let dims = hiddenLayers |> makeLayerSeq x y |> makeThetaDim
    // Could have used x below but using y instead so that its type can be
    // automatically inferred
    let m = y |> Matrix.rowCount
    let thetas = thetaArray |> Matrix.reshape dims
    // calculate activations in each layer by forward propagation
    let activations = thetas |> Seq.scan forwardPropagate x
    // the last activation is the values of the output nodes
    let y' = activations |> Seq.last
    let deltaLast = y' - y
    nnGrad0 m lambda deltaLast activations thetas

  /// <summary>
  /// This function calculates both the cost and the gradient vector of a
  /// multivariate neural network cost function, sharing code and values which
  /// are needed by both to make the computation more efficient
  /// </summary>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="hiddenLayers"></param>
  /// <param name="lambda"></param>
  /// <param name="thetaArray"></param>
  let nnCostGrad x y hiddenLayers lambda thetaArray = 
    let dims = hiddenLayers |> makeLayerSeq x y |> makeThetaDim
               
    // Could have used x below but using y instead so that its type can be
    // automatically inferred
    let m = y |> Matrix.rowCount
    let thetas = thetaArray |> Matrix.reshape dims
    let activations = thetas |> Seq.scan forwardPropagate x
    let y' = activations |> Seq.last
    let deltaLast = y' - y
    let reg = 
      thetas
      // Exclude bias nodes in regularization, hence the slice
      |> Seq.fold (fun s m -> s + (m.[0.., 1..] .^ 2.0 |> Matrix.sum)) 0.0
      |> (*) (lambda / 2.0 / float m)
    ((-y .* log y') - (1.0-y) .* log (1.0 - y') |> Matrix.sum)/(float m) + reg,
    nnGrad0 m lambda deltaLast activations thetas

  /// <summary>
  /// Randomly initializes a vector of a particular size n
  /// </summary>
  /// <param name="l"></param>
  /// <param name="u"></param>
  /// <param name="n"></param>
  let randomInitVector l u n =
    let dist = ContinuousUniform(l, u)
    CreateVector.Random<float>( n, dist )

  /// <summary>
  /// Randomly initializes a matrix of a particular dimension with a continuous
  /// uniform distribution with specified lower and upper bound
  /// </summary>
  /// <param name="l">Lower bound of the continuous uniform distribution</param>
  /// <param name="u">Upper bound of the continuous uniform distribution</param>
  /// <param name="r">Number of rows</param>
  /// <param name="c">Number of columns</param>
  let randomInitMatrix l u r c =
    let dist = ContinuousUniform(l, u)
    CreateMatrix.Random<float>( r, c, dist )

  /// <summary>
  /// Randomly initializes a matrix of a particular dimension with a continuous
  /// uniform distribution which is symmetric around 0 and between [-e, e]
  /// </summary>
  /// <param name="e">The random numbers generated will be in [-e, e]</param>
  /// <param name="r">Number of rows</param>
  /// <param name="c">Number of columns</param>
  let randomInitSymmetric e r c =
    randomInitMatrix (- (abs e)) (abs e) r c

  /// <summary>
  /// Randomly initializes a vector representing the unrolled thetas to be
  /// passed to the optimizer as initial guesses
  /// </summary>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="hiddenLayers"></param>
  /// <param name="epsilon"></param>
  let initTheta x y hiddenLayers epsilon =
    hiddenLayers
    |> makeLayerSeq x y
    |> makeThetaDim
    |> Seq.map (fun (r, c) -> r*c)
    |> Seq.sum
    |> (fun n -> randomInitVector -(abs epsilon) (abs epsilon) n)
    |> Vector.toArray

  /// <summary>
  /// Given the input and output matrix x and y which are the training examples
  /// and their actual values respectively, create a triplet containing the
  /// cost function, the gradient function and a randomly initialized array
  /// The triplet can be piped into the bfgs function directoy for optimization
  /// </summary>
  /// <param name="x">The feature matrix of all training examples</param>
  /// <param name="y">The result of all training examples</param>
  /// <param name="hiddenLayers">Sequence of integers representing the number of
  /// nodes in each hidden layer, not including the input layer and the output
  /// layer as these can be derived using the input and output matrix and also
  /// excluding the bias node in each hidden layer of the network</param>
  /// <param name="lambda">Regularization parameter</param>
  /// <param name="epsilon">The random numbers generated are in the closed
  /// interval [-epsilon, epsilon]</param>
  let makeNN x y hiddenLayers lambda epsilon =
    (
      nnCost x y hiddenLayers lambda,
      nnGrad x y hiddenLayers lambda,
      initTheta x y hiddenLayers epsilon
    )

  /// <summary>
  /// Same as makeNN except that it returns a function which returns both the
  /// cost and the gradient in a tuple
  /// </summary>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="hiddenLayers"></param>
  /// <param name="lambda"></param>
  /// <param name="epsilon"></param>
  let makeNN1 x y hiddenLayers lambda epsilon =
    (
      nnCostGrad x y hiddenLayers lambda,
      initTheta x y hiddenLayers epsilon
    )

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

  /// <summary>
  /// Converts a neural network output matrix, where, the number of rows is 
  /// equal to the number of examples, to a label vector, each element of which
  /// is the zero-based index of the maximum among the values of each output
  /// </summary>
  /// <param name="y"></param>
  let nnOutputToLabels (y: Matrix<float>) = 
    y |> Matrix.aggRows (Vector.maxIndex >> float)

  /// <summary>
  /// Converts the output of a neural network to a binary vector.  If the value
  /// of the output is larger than or equal to 0.5, convert to 1.0
  /// otherwise, 0.0
  /// </summary>
  /// <param name="y">The output of the neural network, assumed to be a single
  /// column matrix (Using a matrix here instead of a vector is to be consistent
  /// with nnOutputToLabels) The rest of the columns is ignored if y has more
  /// than one</param>
  let nnOutputToBinary (y: Matrix<float>) =
    y.[0.., 0]
    |> Vector.map (fun v -> if v >= 0.5 then 1.0 else 0.0)

  /// <summary>
  /// Calculates the number of labels given a label vector.  The largest number
  /// in the vector is assumed to be the largest index of a zero-based vector
  /// representing one output.  Therefore, the actual number of labels is that
  /// plus 1
  /// </summary>
  /// <param name="labelVector"></param>
  let calcNumberOfLabels (labelVector: Vector<float>) =
    labelVector |> Vector.max |> int |> ((+) 1)

  /// <summary>
  /// Converts a vector of labels to the output matrix of a neural network given
  /// the number of labels in the problem. Note the special case when the number
  /// of labels is equal to 2, the number of columns required is 1 not 2 as we 
  /// can then use the value of 0 and 1 in one single column to tell the labels
  /// apart
  /// </summary>
  /// <param name="labels"></param>
  let nnLabelsToOutput numLabels (labels: Vector<float>) =
    let n' = labels |> calcNumberOfLabels
    if numLabels < n' then
      failwithf "The label vector has an element as large as %d, expecting n 
to be %d" (n'-1) n'
    let m = labels |> Vector.length
    let numColsInY = if numLabels <= 2 then 1 else numLabels
    let y = DenseMatrix.create m numColsInY 0.0
    labels 
    |> Vector.iteri (
      fun i v -> 
        if numColsInY = 1 then y.[i, 0] <- v else y.[i, int v] <- 1.0)
    y

  /// <summary>
  /// Predicts by forward propagation given the input and the thetas
  /// </summary>
  /// <param name="x"></param>
  /// <param name="thetas"></param>
  let predictNN x thetas = thetas |> Seq.fold forwardPropagate x

  /// <summary>
  /// Calculates the accuracy of a prediction given the input feature matrix
  /// assuming any normalization has already been carried out and the output
  /// vector containing the expected labels
  /// </summary>
  /// <param name="x"></param>
  /// <param name="y"></param>
  /// <param name="thetas"></param>
  let accuracy x (expected: Vector<float>) thetas =
    let y' = predictNN x thetas
    let y'' =
      match y'.ColumnCount with
      | 1 -> nnOutputToBinary y'
      | _ -> nnOutputToLabels y'
    let numErr = 
      y'' - expected 
      |> abs 
      |> (fun x -> x.PointwiseSign()) 
      |> Vector.sum
    (1.0 - numErr/float (x.RowCount)) * 100.0

  /// <summary>
  /// Implements the workflow of a neural network study, including randomizing
  /// a the rows in the data set matrix, then splitting it up into the training,
  /// cross validation and test data sets according to given percentages and 
  /// then perform the training using the training set.  When done, it uses the
  /// thetas matrix on the cv and test data set to calculate accuracy for all
  /// three data sets.
  /// </summary>
  /// <param name="hidden"></param>
  /// <param name="lambda"></param>
  /// <param name="epsilon"></param>
  /// <param name="trainingPerc"></param>
  /// <param name="testPerc"></param>
  /// <param name="tolerance"></param>
  /// <param name="useNormalization"></param>
  /// <param name="randomize"></param>
  /// <param name="dataSet"></param>
  let runNN hidden lambda epsilon trainingPerc testPerc tolerance 
    useNormalization randomize dataSet =

    // Use the last column of the data set to find the number of labels
    let (_, labelVector) = dataSet |> useLastColAsY
    let numLabels = labelVector |> calcNumberOfLabels

    // split data set into training, test and cross-validation set
    let (training, test, cv) = 
      dataSet
      |> (fun x -> if randomize then randomPermuteMatrixRows x else x)
      |> splitDataSet trainingPerc testPerc

    let (xTrain, labelTrain) = training |> useLastColAsY
    let yTrain = labelTrain |> nnLabelsToOutput numLabels
    let dims = hidden |> makeLayerSeq xTrain yTrain |> makeThetaDim
    let (mu, sd, xTrainNorm) = 
      if useNormalization then 
        let (mu', sd', xnorm) = xTrain |> featureNormalize 
        Some mu', Some sd', xnorm
      else
        (None, None, xTrain)
    
    // Run optimization for thetas
    let thetas = 
      makeNN1 xTrainNorm yTrain hidden lambda epsilon
      |> bfgs1 tolerance
      |> Matrix.reshape dims 
      |> Seq.toArray

    let optionalNormalize (mu, sd) x =
      match (mu, sd) with
      | Some mu', Some sd' -> normalize mu' sd' x
      | _ -> x

    let calcAccuracy (m: Matrix<float> option) =
      match m with
      | Some test ->
          let (xTest, labelTest) = test |> useLastColAsY
          let xnorm = optionalNormalize (mu, sd) xTest
          accuracy xnorm labelTest thetas
      | _ -> -1.0

    (calcAccuracy (Some training),  // just to use the same function
     calcAccuracy test, 
     calcAccuracy cv,
     thetas)

module UnitTests =

  open NUnit.Framework
  open FsUnit
  open MathNet.Numerics.LinearAlgebra
  open ML

  [<TestCase("5", 3, 3, 5, 0.0)>]
  [<TestCase("45", 30, 10, 5, 0.0)>]
  [<TestCase("10, 5", 3, 3, 5, 0.0)>]
  [<TestCase("25, 10, 5", 3, 3, 5, 0.0)>]
  [<TestCase("25, 10, 5", 10, 3, 2, 0.0)>]
  let ``NN Gradient`` (layerString: string, nFeatures, nLabels, m, lambda) =

    let n = if nLabels <= 2 then 1 else nLabels
    let hiddenLayers = 
      layerString.Split([|','|]) 
      |> Array.map (System.Int32.Parse) 
      |> Array.toList

    let initTheta (r, c) =
      [1.0..(float)(r*c)] 
      |> vector 
      |> sin 
      |> Matrix.reshapeVector [(r, c)]
      |> Seq.head

    let x = initTheta (m, nFeatures)
    let y =
      let a = CreateMatrix.Dense(m, n)
      [|1.0..(float m)|]
      |> Matrix.reshape [(m, 1)]
      |> Seq.head
      |> fun u -> u % (float nLabels)
      |> Matrix.iteri(
          fun r c v -> if n = 1 then a.[r, 0] <- v else a.[r, (int v)] <- 1.0)
      a
    let dims = hiddenLayers |> makeLayerSeq x y |> makeThetaDim
    let thetas = dims |> Seq.map initTheta

    let (costFunc, gradFunc, _) = makeNN x y hiddenLayers lambda 0.001
    let numGrad = 
      thetas |> Matrix.unroll |> computeNumericalGradient costFunc |> vector
    let grad = thetas |> Matrix.unroll |> gradFunc |> vector
    (numGrad - grad)
    |> Vector.sum
    |> should be (lessThan 1e-04)

  [<TestCase("25, 10, 5", 10, 3, 2, 0.0)>]
  let ``NN Cost Grad`` (layerString: string, nFeatures, nLabels, m, lambda) =
    let n = if nLabels <= 2 then 1 else nLabels
    let hiddenLayers = 
      layerString.Split([|','|]) 
      |> Array.map (System.Int32.Parse) 
      |> Array.toList

    let initTheta (r, c) =
      [1.0..(float)(r*c)] 
      |> vector 
      |> sin 
      |> Matrix.reshapeVector [(r, c)]
      |> Seq.head

    let x = initTheta (m, nFeatures)
    let y =
      let a = CreateMatrix.Dense(m, n)
      [|1.0..(float m)|]
      |> Matrix.reshape [(m, 1)]
      |> Seq.head
      |> fun u -> u % (float nLabels)
      |> Matrix.iteri(
          fun r c v -> if n = 1 then a.[r, 0] <- v else a.[r, (int v)] <- 1.0)
      a
    let dims = hiddenLayers |> makeLayerSeq x y |> makeThetaDim
    let thetas = dims |> Seq.map initTheta |> Matrix.unroll

    let costFunc = nnCost x y hiddenLayers lambda
    let gradFunc = nnGrad x y hiddenLayers lambda
    let costGradFunc = nnCostGrad x y hiddenLayers lambda

    let c, g = costFunc thetas, gradFunc thetas
    let c', g' = costGradFunc thetas
    printfn "%A" (c, g)
    printfn "%A" (c', g')
    c' |> should equal c
    g' |> should equal g
