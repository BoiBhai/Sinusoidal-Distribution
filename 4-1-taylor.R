h = function(x, s, order=20) {
  n = 1 + 2*(0:order)
  terms = (-1)^floor(n/2) * (pi*x^s)^n / factorial(n)
  sum(terms)
}

g = Vectorize(function(x, s,k, order=4) {
  h(x,s,order)^k
}, vectorize.args='x')

curve(sinu(x, 2,2), xlim=c(0,1))
curve(g(x, 2,2), col='red', add=T)

f = function(n, x) {
  if (n==1) {
    pi * x*s
  } else {
    (-1)^floor(n/2) * f(1, x)^n / factorial(n)
  }
}

sum_f = function (depth, outer, x) {
  i = 0:outer
  sum(choose(outer,i) * f(depth, x))
}


