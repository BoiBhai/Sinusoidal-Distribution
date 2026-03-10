dgammaSum = Vectorize(function(x, nl1, nl2) {
  dgamma(x, nl1[1], nl1[2]) + dgamma(x, nl2[1], nl2[2])
}, vectorize.args='x')