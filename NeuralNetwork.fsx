#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"
#r @"MathNet.Numerics.Data.Text.dll"
#r @"MathNet.Numerics.Data.Matlab.dll"
#r @"DotNumerics.dll"

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Storage
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Matlab
open MathNet.Numerics
open DotNumerics.Optimization
open QuantFin.ML
open System.IO

Control.NativeProviderPath <- Path.Combine [|__SOURCE_DIRECTORY__; @"bin\Debug"|]
Control.UseNativeMKL();;

let path = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex4/ex4/ex4data1.mat"
let thetaPath = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex4/ex4/ex4weights.mat"
let data = MatlabReader.ReadAll<double>(path)
let weights = MatlabReader.ReadAll<double>(thetaPath)
let x : Matrix<float> = data.["X"]
let y0 : Matrix<float> = data.["y"] % 10.0 // convert to zero-based
let dataSet = x |> Matrix.appendCol y0.[0.., 0]
let Theta1 : Matrix<float> = weights.["Theta1"]
let Theta2 : Matrix<float> = weights.["Theta2"]
let nLabels = 10
let y = y0.[0.., 0] |> nnLabelsToOutput nLabels

let hidden = [25; 10]
let lambda = 1.0
let epsilon = 0.001     // used in random initialization
let tolerance = 0.0001  // used in BFGS optimization
let trainingPerc = 0.8
let testPerc = 0.2
let useNormalization = false
let randomize = true

let initThetas = initTheta x y hidden epsilon

let testCostGradPerf () =
  let f = nnCostGrad x y hidden lambda
  (f, initThetas) |> bfgs1 tolerance

let testGradPerf () =
  let f = nnCost x y hidden lambda
  let g = nnGrad x y hidden lambda
  (f, g, initThetas) |> bfgs tolerance

runNN hidden lambda epsilon trainingPerc testPerc tolerance useNormalization randomize dataSet
