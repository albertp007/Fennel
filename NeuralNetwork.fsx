﻿#I @"bin\Debug"
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
let X0 : Matrix<float> = data.["X"]
let y0 : Matrix<float> = data.["y"]
let Theta1 : Matrix<float> = weights.["Theta1"]
let Theta2 : Matrix<float> = weights.["Theta2"]