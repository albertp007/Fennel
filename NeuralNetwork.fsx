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
let y0 : Matrix<float> = data.["y"]
let Theta1 : Matrix<float> = weights.["Theta1"]
let Theta2 : Matrix<float> = weights.["Theta2"]
let nLabels = 10.0
let y =
  let m = CreateMatrix.Dense(y0.RowCount, int nLabels)
  y0
  |> Matrix.iteri (fun r c v -> m.[r, (int v)-1] <- 1.0)
  m

let hiddenLayers = [25]
let dims = hiddenLayers |> makeLayerSeq x y |> makeThetaDim
let randomMatrix (r, c) = DenseMatrix.randomStandard<float> r c

let lambda = 1.0
let epsilon = 0.001     // used in random initialization
let tolerance = 0.0001  // used in BFGS optimization

let predict x thetas =
  thetas
  |> Seq.fold forwardPropagate x

let accuracy x y thetas =
  predict x thetas
  |> Matrix.aggRows (Vector.maxIndex >> float)
  |> ((+) 1.0)
  |> Seq.map2 (fun x y -> (x = y) |> boolToFloat) y
  |> Seq.sum
  |> float
  |> ((/) (float x.RowCount))
  |> ((/) 1.0)
  |> ((*) 100.0)

let thetas = 
  makeNN x y hiddenLayers lambda epsilon 
  |> bfgs tolerance
  |> Matrix.reshape dims 
  |> Seq.toArray

accuracy x y0.[0.., 0] thetas

// let names = [|"Theta1"; "Theta2"|]
// MatlabWriter.Write("thetas.mat", thetas, names)
