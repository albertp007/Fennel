#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"
#r @"MathNet.Numerics.Data.Text.dll"
#r @"MathNet.Numerics.Data.Matlab.dll"
#r @"DotNumerics.dll"

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Matlab
open DotNumerics.Optimization
open QuantFin.ML

let path = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex3/ex3/ex3data1.mat"
let thetaPath = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex3/ex3/allTheta.mat"
let data = MatlabReader.ReadAll<double>(path)
let X0 : Matrix<float> = data.["X"]
let y = data.["y"].[0.., 0]
let X = X0 |> Matrix.prependOnes

let inline (.=) m f =
    Matrix.map (((=) f) >> (fun b -> if b then 1.0 else 0.0)) m

let nPatterns = 10
let m = X.RowCount
let n = X.ColumnCount
let lambda = 0.1
let alpha = 1.0

let predict (allTheta: Matrix<float>) (v: Vector<float>) =
  let v' = Vector.join (vector [1.0] ) v
  allTheta * v'
  |> Vector.sigmoid
  |> (fun v -> (v |> Vector.maxIndex) + 1, v |> Vector.max)

let rand = System.Random()

let trainGradientDescent n lambda (x: Matrix<float>) (y: Vector<float>) alpha k =
  let th0 = DenseVector.create n 0.0
  let y1 = y |> Vector.map ((=) (float k) >> boolToFloat)
  gradientDescent n alpha (lgrGradCost lambda X y1) th0


let trainBFGS lambda (x: Matrix<float>) (y: Vector<float>) k =
  let th0 = DenseVector.create n 0.0 |> Vector.toArray
  // interfaces to the bfgs object via conversion to float array
  let y' = y |> Vector.map ((=) (float k) >> boolToFloat)
  let f (t: float[]) = t |> vector |> lgrCost lambda x y'
  let g (t: float[]) = t |> vector |> lgrGrad lambda x y' |> Vector.toArray
  let optimizer = L_BFGS_B()
  optimizer.ComputeMin( f, g, th0 )
  |> vector

let accuracy (y: Vector<float>) (yhat: Vector<float>) =
  let m = y.Count
  yhat - y |> abs |> Vector.map (sign >> float) |> Vector.sum
  |> (fun f -> 1.0 - f/float m)
  
let allTheta =
  if System.IO.File.Exists(thetaPath) then
    MatlabReader.Read<float>(thetaPath, "allTheta")
  else
    [1..10]
    |> List.map (trainGradientDescent 400 0.1 X y 1.0 )
    |> List.map fst
    |> matrix

let allThetaBfgs =
  if System.IO.File.Exists(thetaPath) then
    MatlabReader.Read<float>(thetaPath, "allThetaBfgs")
  else
    [1..10]
    |> List.map (trainBFGS lambda X y )
    |> matrix

rand.Next(5000) |> (fun i -> X0.[i, 0..] |> predict allTheta, y.[i])
let yhat = 
  X * allTheta.Transpose() 
  |> Matrix.sigmoid 
  |> Matrix.aggRows (Vector.maxIndex >> float >> ((+) 1.0))

let yhatBfgs = 
  X * allThetaBfgs.Transpose() 
  |> Matrix.sigmoid 
  |> Matrix.aggRows (Vector.maxIndex >> float >> ((+) 1.0))

yhat |> accuracy y;;
yhatBfgs |> accuracy y;;