Chi2_dist = function(p, q, w, masspoints) {
  summand = function(x) (p(x)-q(x))^2/w
  sum = 0
  for (x in masspoints) sum = sum+summand(x)
  return(sum)
}

symmChi2_dist = function(p, q, masspoints) {
  Chi2_dist(p,q, w=function(x) 1/2*(p(x)+q(x)), masspoints)
}