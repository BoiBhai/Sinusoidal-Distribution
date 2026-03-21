a1=0; d1=1; s1=1; k1=4
a2=3; d2=4; s2=1; k2=7
adddens = Vectorize(function(y) {
  integrand = function(t) dsinu(t, a1,d1,s1,k1)*dsinu(y-t,a2,d2,s2,k2)
  integrate(integrand, lower=0, upper=1)$value
})

support1 = c(max(a1,a2), a1+d1+a2+d2)
curve(adddens(x), xlim=support1)
optim1 = fit.sinug.Hel(adddens, support_g=support1)
par1 = optim1$par
curve(dsinu(x,par1[1], par1[2], par1[3], par1[4]), add=T, col='red')
