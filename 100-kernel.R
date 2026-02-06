source('1-sinudist.R')

sinu.kernel = function(data, h = 1, s = 1, k = 1) {
  n = length(data)
  a_vals = data - h
  d_val = 2 * h
  
  kernel = function(x) {
    sum(vapply(a_vals, function(a) dsinu(x, a, d_val, s, k), numeric(1))) / (n)
  }
  
  return(Vectorize(kernel))
}


data1 = rnorm(1000)
plot(density(data1))
kde1 = sinu.kernel(data1, h=0.2, s=1, k=1)
curve(kde1, add=T, col='red')

