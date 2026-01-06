fitsinu.hel.gamma = function(n, lambda, switch=F) {
  PDF.str = paste0('dgamma(x,',n,',',lambda,')')
  fit1 = fitsinu.hel(PDF.str, switch)
  out=fit1$par
  aOut = out[1]; dOut=out[2]; oOut=out[3]; cOut=out[4]
  par(mar=c(0,0,2,2))
  curve(dgamma(x,n, lambda), xlim=c(aOut, aOut+dOut))
  curve(dsinu(x, aOut, dOut, oOut, cOut, switch), add=T, col='red')
}

par(mfrow=c(3,5))

n.vec=c(1,5,10); lambda.vec=c(2,4,6,8,10)

for (n in n.vec) {
  for (lambda in lambda.vec) {
    fitsinu.hel.gamma(n,lambda)
  }
}