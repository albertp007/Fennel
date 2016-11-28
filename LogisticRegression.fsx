#I @"bin\Debug"
#I @"packages"
#r @"QuantFin.dll"
#r @"MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.dll"
#r @"MathNet.Numerics.Data.Text.dll"
#r @"MathNet.Numerics.Data.Matlab.dll"

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Data.Text
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Matlab
open QuantFin.ML

let path = "/Users/panga/Dropbox/Machine Learning/machine-learning-ex3/ex3/ex3data1.mat"
let data = MatlabReader.ReadAll<double>(path)
let X0 = data.["X"]
let y = data.["y"].[0.., 0]
let X = X0 |> Matrix.prependOnes

let inline (.=) m f =
    Matrix.map (((=) f) >> (fun b -> if b then 1.0 else 0.0)) m

let nPatterns = 10
let m = X.RowCount
let n = X.ColumnCount
let lambda = 0.1
let k = 2.0
let y1 = y |> Vector.map ((=) k >> boolToFloat)
let th0 = DenseVector.create n 0.0
lgrGrad lambda X y th0
