fitsinu.ecdf.curve = function (data, str='', flip=F) {
  fit1 = fitsinu.ecdf(data, flip)
  par1 = fit1$par
  
  ecdf1 = ecdf(data)
  curve(ecdf1(x), xlim=c(min(data), max(data)), col='green', lwd=5)
  curve(psinu(x, par1[1], par1[2], par1[3], par1[4], flip), add=T, col='red', lwd=2)
  
  pars = paste(round(par1, 1), collapse=', ')
  
  if (str=='')
    str = deparse(substitute(data))
  sinu.str = paste0('Sinu(',pars,')')
  legend('bottomright', legend=c(str, sinu.str), fill=c('green','red'))
}

par(mfrow=c(2,2))
par(mar=c(2,2,0,0))

### Binomial
fitsinu.ecdf.curve(rbinom(100, 342, 0.3), '100x Binom (342, 0.3)')

### Poisson 
fitsinu.ecdf.curve(rpois(100, 2.6), '100x Poi (2.6)')

### Cars
data1 = read.csv('cars.csv')
price1 = sample(data1$price, 100)
fitsinu.ecdf.curve(price1, '100x cars$price')

### Income
data1 = read.csv('income.csv')
fnlwgt1 = sample(data1$fnlwgt, 100)
fitsinu.ecdf.curve(fnlwgt1, '100x income$fnlwgt')