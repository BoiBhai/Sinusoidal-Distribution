library(MASS)

############################################################
# Simple dataset
############################################################

data(cars)

y = cars$dist

X = model.matrix(
  dist ~ speed,
  data = cars
)

n = length(y)
p = ncol(X)

############################################################
# OLS initialization
############################################################

ols = lm(dist ~ speed, data = cars)

beta.init = coef(ols)

resid.init = residuals(ols)

a.init = min(resid.init)
d.init = max(resid.init) - min(resid.init)

s.init = 1
k.init = 1

############################################################
# Negative log-likelihood
############################################################

nll = function(par) {
  
  beta = par[1:p]
  
  a = par[p+1]
  d = par[p+2]
  s = par[p+3]
  k = par[p+4]
  
  if(d <= 0 || s <= 0 || k <= 0)
    return(1e12)
  
  resid = y - X %*% beta
  
  dens = dsinu(
    resid,
    a = a,
    d = d,
    s = s,
    k = k
  )
  
  if(any(!is.finite(dens)) || any(dens <= 0))
    return(1e12)
  
  -sum(log(dens))
  
}

############################################################
# Optimization
############################################################

par.init = c(
  beta.init,
  a.init,
  d.init,
  s.init,
  k.init
)

fit = optim(
  par = par.init,
  fn = nll,
  method = "L-BFGS-B",
  lower = c(
    rep(-Inf, p),
    -Inf,
    1e-6,
    1e-6,
    1e-6
  )
)
fit

############################################################
# Results
############################################################

beta.hat = fit$par[1:p]

a.hat = fit$par[p+1]
d.hat = fit$par[p+2]
s.hat = fit$par[p+3]
k.hat = fit$par[p+4]

cat("\nEstimated regression coefficients:\n")
print(beta.hat)

cat("\nEstimated Sinu parameters:\n")
cat("a =", a.hat, "\n")
cat("d =", d.hat, "\n")
cat("s =", s.hat, "\n")
cat("k =", k.hat, "\n")

cat("\nNegative log-likelihood:\n")
print(fit$value)