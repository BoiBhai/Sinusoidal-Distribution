dpd = function(tuning, pdf, targetPDF, support=c(-Inf, Inf)) {
  integrand = function(x) {
    if (tuning>0) {
      pdf(x)^(1+tuning) - (1+1/tuning)*pdf(x)^tuning*targetPDF(x) + 1/tuning*targetPDF(x)^(1+tuning)
    } else if (tuning==0) {
      targetPDF(x) * (log(targetPDF(x)) - log(pdf(x)))
    } else {
      warning('tuning parameter cannot be <0')
    }
  }
  integrate(integrand, lower=support[1], upper=support[2])
}

dpd_optim = function(tuning, pdf, targetPDF, support=c(-Inf, Inf)) {
  integrand = function(x) {
    if (tuning>0) {
      pdf(x)^(1+tuning) - (1+1/tuning)*pdf(x)^tuning*targetPDF(x)
    } else if (tuning==0) {
      - targetPDF(x) * log(pdf(x))
    } else {
      warning('tuning parameter cannot be <0')
    }
  }
  integrate(integrand, lower=support[1], upper=support[2])
}


fitsinu.dpd = function(tuning, targetPDF, flip=F, support=c(-Inf,Inf), init.par=c(0,1,1,1), lower.par=c(-500,0.01,0.01,0.01), upper.par=c(500,1000,100,100)) {
  dpd_loss = function(pars) {
    a=pars[1]; d=pars[2]; s=pars[3]; k=pars[4]
    pdf = function(x) dsinu(x, a,d,s,k,flip)
    dpd_optim(tuning, pdf, targetPDF, support=support)$value
  }
  optim1 = optim(init.par, dpd_loss, method='L-BFGS-B', lower=lower.par, upper=upper.par)
  pars = optim1$par
  optim1$sindist = paste0('dsinu(x,', paste(pars[1], pars[2], pars[3], pars[4], flip, sep=','), ')')
  return(optim1)
}


fitsinu.dpd.curve = function(tuning, targetPDF, targetPDF.str=NA, flip=F, xlim=c(-3,3)) {
  ### fitting
  fit1 = fitsinu.dpd(tuning, targetPDF, flip=flip)
  sindist = fit1$sindist
  
  pdf = fos2fun(sindist)
  
  ### curve plotting
  dpd.txt = paste0('DPD = ', dpd(tuning, pdf, targetPDF)$value)
  curve(targetPDF(x), col='green', lwd=5, xlim = xlim, main=dpd.txt, xlab=sindist, ylab=targetPDF.str)
  curve(pdf(x), add=T, col='red', lwd=2)
  
  pars=fit1$par
  moms = sinu.msm(pars[1], pars[2], pars[3], pars[4], flip)
  legend('topleft', legend=moms, fill=c('grey', 'blue', 'red', 'green'))
  
  return(fit1)
}

fitsinu.dpd.curve(1, 'dnorm(x, 4, 5)', xlim=c(-11,19))
fitsinu.dpd.curve(1, 'dexp(x, 3)')

fitsinu.pdf.curve('dgamma(x, 5, 9)', xlim=c(0,1.5))
fitsinu.dpd.curve(1, 'dgamma(x, 5,9)', xlim=c(0,1.5))

fitsinu.pdf.curve('dt(x, 5)')
fitsinu.dpd.curve(1, 'dt(x, 5)')

fitsinu.dpd.hist = function(tuning, data, targetPDF.str=NA, flip=F, xlim=c(min(data), max(data))) {
  normdata = (data - min(data))/(max(data) - min(data))
  dens = density(data)
  normdens = density(normdata)
  targetPDF = approxfun(dens, yleft=0, yright=0)
  normtargetPDF = approxfun(normdens, yleft=0, yright=0)
  
  fit1 = fitsinu.dpd(tuning, normtargetPDF, flip=flip)
  pars = fit1$par
  pars[2] = pars[2] * (max(data) - min(data))
  pars[1] = pars[1]*(max(data) - min(data)) + min(data)
  sindist = paste0('dsinu(x,', paste(pars[1], pars[2], pars[3], pars[4], flip, sep=','), ')')
  pdf = fos2fun(sindist)
  
  dpd.txt = paste0('DPD=', dpd(1, pdf, targetPDF)$value)
  hist(data, probability=T, xlim=xlim, main=dpd.txt, xlab=sindist, ylab=targetPDF.str)
  dens = density(data)
  lines(dens, col='green', lwd=5)
  curve(pdf(x), add=T, col='red', lwd=2)
  
  moms = sinu.msm(pars[1], pars[2], pars[3], pars[4], flip)
  legend('topleft', legend=moms, fill=c('grey', 'blue', 'red', 'green'))
  return(fit1)
  
}

fitsinu.dpd.hist(1, chickwts$weight, 'chickwts-weight')
fitsinu.dpd.hist(1, rnorm(1000), 'rnorm 1000x')
