fitsinu.hel.beta = function(alpha, beta, switch=F) {
  PDF.str = paste0('dbeta(x,',alpha,',',beta,')')
  fit1 = fitsinu.hel(PDF.str, switch)
  out=fit1$par
  aOut = out[1]; dOut=out[2]; oOut=out[3]; cOut=out[4]
  par(mar=c(0,0,2,2))
  curve(dbeta(x, alpha,beta), xlim=c(aOut, aOut+dOut))
  curve(dsinu(x, aOut, dOut, oOut, cOut, switch), add=T, col='red')
}

par(mfrow=c(3,5))

alpha.vec=c(1,5,10); beta.vec=c(2,4,6,8,10)

for (alpha in alpha.vec) {
  for (beta in beta.vec) {
    fitsinu.hel.beta(alpha, beta)
  }
}