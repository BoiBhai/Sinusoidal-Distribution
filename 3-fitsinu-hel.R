fos2fun = function(fos) {
  if (class(fos) == 'character') {
    fos.parsed = parse(text=fos)
    fun = function(x) eval(fos.parsed)
  } else {
    fun = fos
  }
  return(fun)
}

helDist = function(pdf, targetPDF, support=c(-Inf, Inf)) {
  #pdf = fos2fun(pdf)
  #targetPDF = fos2fun((targetPDF))
  # both of them need to be functions. whether you need to parse something is up to you, outside the command
  integrand = function(x) sqrt(pdf(x)*targetPDF(x))
  intVal = integrate(integrand, support[1], support[2])
  intVal$value = 1 - intVal$value
  intVal
}

fitsinu.hel = function(targetPDF, flip = FALSE, pgtol=0.01, init.par = c(0, 1, 1, 1), lower.par = c(-1000, 0.001, 0.001, 0.001), upper.par = c(1000, 1000, 1000, 1000)) {
  ### parsing
  heldist <- function(pars) {
    a = pars[1]; d = pars[2]; s = pars[3]; k = pars[4]
    pdf = function(x) dsinu(x, a, d, s, k, flip)
    - helDist(pdf, targetPDF, support=c(a,a+d))$value
  }
  optim1 = optim(init.par, heldist, lower=lower.par, upper=upper.par, method='L-BFGS-B')
  par1 = optim1$par
  optim1$sindist = paste0('dsinu(x,', paste(par1[1], par1[2], par1[3], par1[4], flip, sep=','), ')')
  return(optim1)
}

fitsinu.hel.curve = function(targetPDF, targetPDF.str=NA, flip=F, xlim=c(-3,3), fitsinu.hel.args=list()) {
  ### fitting
  targetPDF = fos2fun(targetPDF)
  
  fit1 = fitsinu.hel(targetPDF, flip=flip, fitsinu.hel.args)
  sindist = fit1$sindist
  
  pdf = fos2fun(sindist)
  
  ### curve plotting
  helDist.txt = paste0('H^2 = ', helDist(pdf, targetPDF))
  curve(targetPDF(x), col='green', lwd=5, xlim = xlim, main=helDist.txt, xlab=sindist, ylab=targetPDF.str)
  curve(pdf(x), add=T, col='red', lwd=2)
  
  pars=fit1$par
  moms = sinu.msm(pars[1], pars[2], pars[3], pars[4], flip)
  legend('topleft', legend=moms, fill=c('grey', 'blue', 'red', 'green'))
  
  return(fit1)
}

fitsinu.hel.curve('dnorm(x)')

fitsinu.hel.hist = function(data, flip=F, xlim=c(min(data), max(data)), pgtol=0) {
  normdata = (data - min(data))/(max(data) - min(data))
  normdens = density(normdata)
  normtargetPDF = approxfun(normdens, yleft=0, yright=0)
  
  dens = density(data)
  hist(data, probability=T, xlim=xlim)
  lines(dens, col='green', lwd=5)
  
  fit1 = fitsinu.hel(normtargetPDF, flip=flip, pgtol=pgtol)
  
  pars = fit1$par
  pars[2] = pars[2] * (max(data) - min(data))
  pars[1] = pars[1]*(max(data) - min(data)) + min(data)
  pdf = function(x) dsinu(x, pars[1], pars[2], pars[3], pars[4])
  
  #sindist = fit1$sindist
  
  #pdf = fos2fun(sindist)
  
  ### curve plotting
  
  curve(pdf(x), add=T, col='red', lwd=2)
  return(fit1)
  
}

fitsinu.hel.hist(rpois(1000, 100))
fitsinu.hel.hist(chickwts$weight, xlim=c(0,500))