sigmoid(z) = 1 ./ (1+exp(-z))

function makeThetaDims(m::Int64, n::Int64, hiddenLayers::Vector{Int64})
  layers=[m; hiddenLayers; n]
  nLayers = length(layers)
  dims = Vector{Tuple{Int64, Int64}}(nLayers-1)
  for i in 1:(nLayers-1)
    dims[i] = (layers[i+1], layers[i]+1)
  end
  return dims
end

import Base.reshape
function reshape(v::Vector{Float64}, dims::Vector{Tuple{Int64, Int64}})
  thetas=Array{Matrix{Float64}}(length(dims))
  start = 1
  for i in 1:length(dims)
    (r, c) = dims[i]
    l = r * c
    thetas[i] = reshape(v[start:(start+l-1)], dims[i])
    start += l
  end
  return thetas
end

function unroll(thetas::Array{Matrix{Float64}}, res::Vector{Float64})
  size = sum( map(thetas) do t length(t) end )
  start = 1
  #v = Array{Float64}(size)
  for i in 1:length(thetas)
    thetaLength = length(thetas[i])
    res[start:(start+thetaLength-1)] = reshape(thetas[i], thetaLength, 1)[:, 1]
    start += thetaLength
  end
end

function unroll(thetas::Array{Matrix{Float64}})
  size = sum( map(thetas) do t length(t) end )
  v = Array{Float64}(size)
  unroll(thetas, v)
  v

function forward(x::Matrix{Float64}, thetas::Vector{Matrix{Float64}})
  m = size(x, 1)
  a = Vector{Matrix{Float64}}(length(thetas))
  h = copy(x)
  for (i, theta) in enumerate(thetas)
    h = sigmoid( [ones(m) h] * theta' )
    a[i] = copy(h)
  end
  return a
end

function labelToVector(n::Int64, labels::Vector{Float64})
  v = zeros(length(labels), n)
  for (i, l) in enumerate(labels)
    v[i, Int(l)] = 1.0
  end
  return v
end

function calcDelta(deltaNext::Matrix{Float64}, theta::Matrix{Float64},
  activation::Matrix{Float64})
  m = size(activation, 1)
  a1 = [ones(m) activation]
  delta = (deltaNext * theta) .* a1 .* (1-a1)
  return delta[:, 2:end]
end

function nnCost(x::Matrix{Float64}, y::Matrix{Float64},
  hiddenLayers::Vector{Int64}, numLabels::Int64, lambda::Float64)
  (m, n) = size(x)
  dims = makeThetaDims(n, numLabels, hiddenLayers)
  v ->
    begin
      thetas = reshape(v, dims)
      a = forward(x, thetas)
      h = last(a)
      reg = 0.0
      for t in thetas
        reg += sum(t[:, 2:end] .^ 2)
      end
      return sum(-y .* log(h) - (1-y) .* log(1-h))/m + 0.5 * lambda * reg / m
    end
end

function nnGrad(x::Matrix{Float64}, y::Matrix{Float64},
  hiddenLayers::Vector{Int64}, numLabels::Int64, lambda::Float64)

  (m, n) = size(x)
  dim = makeThetaDims(n, numLabels, hiddenLayers)
  (v, storage) ->
    begin
      theta = reshape(v, dim)
      a = prepend!(forward(x, theta), [x])
      grad = Array{Matrix{Float64}}(length(a)-1)
      reg = Array{Matrix{Float64}}(length(grad))
      delta = Array{Matrix{Float64}}(length(a))
      delta[end] = last(a) - y
      for i in (length(a)-1):(-1):2
        delta[i] = calcDelta(delta[i+1], theta[i], a[i])
      end
      for i in 1:(length(a)-1)
        grad[i] = delta[i+1]' * [ones(m) a[i]] / m
        reg[i] = [zeros(size(theta[i], 1)) theta[i][:, 2:end]]
        grad[i] += lambda * reg[i] / m
      end
      unroll(grad, storage)
    end
end

function numericalGradient(f, epsilon::Float64, x::Vector{Float64})
  grad = Vector{Float64}(length(x))
  for i in 1:length(x)
    plus = copy(x)
    minus = copy(x)
    plus[i] += epsilon
    minus[i] -= epsilon
    grad[i] = (f(plus) - f(minus))/2/epsilon
  end
  grad
end
