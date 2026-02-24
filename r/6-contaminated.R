f.c = function(x) {
  p = 0.2
  p*dnorm(x,0) + (1-p)*dnorm(x,5)
}

rmom = function(r, pdf, support=c(-Inf, Inf)) {
  integrand = function(x) pdf(x) * x^r
  integrate(integrand, lower=support[1], upper=support[2])
}

cmom = function(r, pdf, support=c(-Inf, Inf)) {
  mu = rmom(1, pdf)$value
  integrand = function(x) pdf(x) * (x-mu)^r
  integrate(integrand, lower=support[1], upper=support[2])
}

rmom(1, f.c)
cmom(2, f.c)


integrate(f.c)

curve(f.c(x), from=-3, to=10)

alpha.vec=c(0.2, 0.4, 0.6, 0.8, 1)

par(mfrow=c(1,5))
for (alpha in alpha.vec) {
  fitsinu.dpd.curve(alpha, 'f.c(x)', xlim=c(-10,10))
}

fitsinu.hel.curve('f.c(x)', '0.2N(0,1) and 0.8N(5,1)', xlim=c(-10,10))
fitsinu.dpd.curve(1, 'f.c(x)', '0.2N(0,1) and 0.8N(5,1)', xlim=c(-10,10))
