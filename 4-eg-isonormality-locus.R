


dCor.k = Vectorize(function(k) {
  1/sqrt(sinu.var(k=k))}, vectorize.args='k')
aCor.k = Vectorize(function(k) {
  -dCor.k(k)/2}, vectorize.args='k')

### isonormality curve and a transformed version
curve(dCor.k(x), xlim=c(0,100), lwd=2, xlab='k', ylab=expression(d~ 'cor' ~ k))
curve(dCor.k(x)^2, xlim=c(0,100), lwd=2, xlab='k', ylab=expression(d^2~ 'cor' ~ k))

### empirical modelling of delta
k.vals = 0:100
dCor.vals = deltaCor.k(k.vals)^2
lm1 = lm(dCor.vals ~ k.vals)
summary(lm1)
plot(k.vals, dCor.vals)
