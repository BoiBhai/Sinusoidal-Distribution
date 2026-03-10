fitsinu.ecdf <- function(data, flip = FALSE, init.par = c(1, 1), lower.par = c(0.001, 0.001), upper.par = c(500, 500), pgtol = 0) {
  a.out <- min(data); d.out <- max(data) - a.out
  data.norm <- (data - a.out) / d.out; ecdfAdj <- ecdf(data.norm)
  
  ks.score <- function(pars) {
    omega <- pars[1]; chi <- pars[2]
    devs <- ecdfAdj(data.norm) - psinu(data.norm, 0, 1, omega, chi, flip)
    return(mean(devs^2))
  }
  
  optim1 <- optim(init.par, ks.score, method = 'L-BFGS-B', lower = lower.par, upper = upper.par, control = list(pgtol = pgtol))
  optim1$par <- c(a.out, d.out, optim1$par)
  
  return(optim1)
}


data1 = chickwts$weight
optim1 = fitsinu.ecdf(data1)
par1 = optim1$par

ecdf1 = ecdf(data1)

plot(ecdf1)
curve(psinu(x, par1[1], par1[2], par1[3], par1[4]), add=T, col='red', lwd=3)
