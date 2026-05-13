n = 50

niter=50

# 1. Data generation
#set.seed(1234)

ser.estimates = matrix(
  NA,
  nrow = niter,
  ncol = 6
)

colnames(ser.estimates) = c(
  "beta0.hat",
  "beta1.hat",
  "a.hat",
  "d.hat",
  "s.hat",
  "k.hat"
)

for (iter in 1:niter) {
  beta0.true = runif(1,10,20)
  beta1.true = runif(1,5,10)
  d.true=2; s.true=3; k.true=5
  a.true = -sinu.mean(0, d.true, s.true, k.true)
  
  x = runif(n)
  
  epsilon = rsinu(n, a.true, d.true, s.true, k.true)
  hist(epsilon)
  y = beta0.true + beta1.true*x + epsilon
  
  ols = lm(y ~ x)
  
  beta0.init = coef(ols)[1]
  beta1.init = coef(ols)[2]
  resid.init = residuals(ols)
  d.init = 2*diff(range(ols$residuals))
  s.init = 1
  k.init = 19
  
  nll = function(par)
  {
    beta0 = par[1]
    beta1 = par[2]
    d     = par[3]
    s     = par[4]
    k     = par[5]
    
    # force mean-zero errors
    a = -sinu.mean(0,d,s,k)
    r = y - beta0 - beta1*x
    dens = dsinu(r,a,d,s,k)
    
    # numerical protection
    if(any(!is.finite(dens)) || any(dens <= 0))
      return(1e12)
    
    -sum(log(dens))
  }
  
  fit = optim(
    par = c(
      beta0.init,
      beta1.init,
      d.init,
      s.init,
      k.init
    ),
    
    fn = nll,
    
    method = "L-BFGS-B",
    
    lower = c(
      -100,
      -50,
      d.init,
      0.1,
      0.1
    ),
    
    upper = c(
      100,
      50,
      10*d.init,
      10,
      20
    )
  )
  print(fit)
  
  beta0.hat = fit$par[1]
  beta1.hat = fit$par[2]
  d.hat     = fit$par[3]
  s.hat     = fit$par[4]
  k.hat     = fit$par[5]
  a.hat = -sinu.mean(0,d.hat,s.hat,k.hat)
  
  ser.estimates[iter, ] = c(
    beta0.hat,
    beta1.hat,
    a.hat,
    d.hat,
    s.hat,
    k.hat
  )
}

# =========================================================
# After loop
# =========================================================

ser.estimates

summary(ser.estimates)

apply(ser.estimates, 2, mean, na.rm=F)

apply(ser.estimates, 2, sd, na.rm=TRUE)