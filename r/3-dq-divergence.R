# f*(F-1(p)) gives density at quantile that is defined over 0,1 only, so it is distribution invariant, and f* ensures it is a density that integrates to 1. It is location and scale invariant and gives the exact shape of the density.

# Define density quantile function as dq for now.

dqdist.unscaled = function(p, ddist, qdist){
  ddist(qdist(p))
}

dqdist.const = function(ddist, qdist){
  dq.u.p = function(p) dqdist.unscaled(p, ddist, qdist)
  const = integrate(dq.u.p, lower=0, upper=1)$value
  return(const)
}

dqdist = function(p, ddist, qdist){
  return(dqdist.unscaled(p, ddist, qdist)/dqdist.const(ddist, qdist))
}

################################################################################

dqnorm = function(p){
  return(dqdist(p, dnorm, qnorm))
}
dqsinu = function(p, s,k){
  dsinu1 = function(z) dsinustd(z,s,k)
  qsinu1 = function(p) qsinustd(p,s,k)
  return(dqdist(p, dsinu1, qsinu1))
}
dqbeta = function(p,a,b){
  dbeta1 = function(z) dbeta(z, a,b)
  qbeta1 = function(p) qbeta(p, a,b)
  return(dqdist(p, dbeta1, qbeta1))
}
dqgamma = function(p, n){
  dgamma1 = function(z) dgamma(z, n,1)
  qgamma1 = function(p) qgamma(p, n,1)
  return(dqdist(p, dgamma1, qgamma1))
}



curve(dqsinu(x, 1,100))
curve(dqnorm(x), col='red', add=T)
curve(dqbeta(x, 100,100), col='green', add=T)
curve(dqgamma(x, 1000), col='blue', add=T)

integrate(dqnorm, lower=0, upper=1)