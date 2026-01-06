###fitsinu.mms.hist for PMFs
fitsinu.msm.hist = function (data, str='', flip=F) {
  fit1 = fitsinu.msm(target)
  par1 = fit1$par

  if (str=='')
    str = deparse(substitute(data))
  hist(data, prob=T, xlab='x', main='')
  curve(dsinu(x, par1[1], par1[2], par1[3], par1[4], flip), add=T, col='red', lwd=3)
  
  pars = paste(round(par1, 1), collapse=', ')
  sinu.str = paste0('Sinu(',pars,')')
  legend('topright', legend=c(str, sinu.str), fill=c('grey','red'))
}

### fitsinu.mms.curve for PDFs
fitsinu.msm.curve = function(dist, str, flip=F, xlim=c(-3,3)) {
  parse1 = parse(text=dist); pdf = function(x) eval(parse1)
  
  fit1 = fitsinu.msm(target, flip=flip)
  par1 = fit1$par
  
  curve(pdf(x), xlim=xlim, ylab='Density', col='green', lwd=5)
  curve(dsinu(x, par1[1], par1[2], par1[3], par1[4], flip), add=T, col='red', lwd=2)
  
  pars = paste(round(par1, 1), collapse=', ')
  sinu.str = paste0('Sinu(',pars,')')
  legend('topright', legend=c(str, sinu.str), fill=c('green','red'))
}


### FITTING ###

par(mfrow=c(1,3))

### BINOMIAL
n=30; p=0.8; q=1-p
target=c(n*p, n*p*q, (q-p)/sqrt(n*p*q), (1-6*p*q)/(n*p*q))
fitsinu.msm.hist(rbinom(1000, n,p), str='1000x Binom(30, 0.8)')

### Poisson
lambda = 5.6
target=c(lambda, lambda, 1/sqrt(lambda), 1/lambda)

fitsinu.msm.hist(rpois(1000, lambda), str='1000x rpois(5.6)')

### chickwts
library(agricolae)
chickData = chickwts$weight
target= c(mean(chickData), var(chickData), skewness(chickData), kurtosis(chickData))
fitsinu.msm.hist(chickData, str='Weights from chickwts dataset')

### 
par(mfrow=c(3,2))

### NORMAL
target=c(0, 1, 0, 0)
fitsinu.msm.curve('dnorm(x,0,1)', 'N(0,1)')

### Chi squared
n = 13
target = c(n, 2*n, sqrt(8/n), 12/n)
fitsinu.msm.curve('dchisq(x,n)', expression({chi^2}(13)), xlim=c(0,40), flip=T)

### t
n = 6
target = c(0, n/(n-2), 0, 6/(n-4))
fitsinu.msm.curve('dt(x,n)', 't(13)', xlim=c(-3,3))

### Exponential
lambda=5
target=c(1/lambda, 1/lambda^2, 2, 6)
fitsinu.msm.curve('dexp(x,lambda)', 'Exp(5)', xlim=c(0,1))

### GAMMA
n=8; lambda=5
target=c(n/lambda, n/lambda^2, 2/sqrt(n), 6/n)
fitsinu.msm.curve('dgamma(x,n,lambda)', 'Gamma(8,5)', xlim=c(0,4), flip=T)

### BETA
m=17; n=6
skew.t = 2*(n-m)*sqrt(m+n+1)/((m+n+2)*sqrt(m*n))
kurt.t = 6 *((m-n)^2 * (m+n+1) - m*n*(m+n+2) )/(m*n * (m+n+2) * (m+n+3))
target = c(m/(m+n), (m*n)/((m+n)^2*(m+n+1)), skew.t, kurt.t)
fitsinu.msm.curve('dbeta(x,m,n)', 'Beta(2,3)', xlim=c(0.4,1.2), flip=T)
