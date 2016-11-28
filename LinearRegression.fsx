#load "packages/Deedle.1.2.5/Deedle.fsx"
#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"
#r @"MathNet.Numerics.Data.Text.dll"

open System
open Deedle
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open QuantFin.ML

let linearRegress (x: Series<'a, float>) y =
  Series.zipInner x y
  |> Series.values
  |> MathNet.Numerics.LinearRegression.SimpleRegression.Fit

let grad f g x y = f x y, g x y

// reading a csv file
let data = DelimitedReader.Read<float>( "/Users/panga/Dropbox/ex1data2.txt", 
             false, ",", false)
let m = data.RowCount
let n = data.ColumnCount
// append column of all 1.0 as x0
let (mu, sigma, X') = data.[0.., 0..(n-2)] |> featureNormalize
let X = DenseVector.create m 1.0 |> Matrix.prependCol <| X'
let y = data.[0.., (n-1)]
let theta0 = vector [0.0; 0.0; 0.0]
let theta = gradientDescent 1500 0.01 (lnrGrad 0.0 X y >> fst) theta0
let v = vector [1650.0; 3.0]
(v - mu)/sigma |> Vector.join (vector [1.0]) |> (*) theta
