### fitsinu.hel.curve for PDFs

par(mfrow=c(2,3))

### Normal
fit1 = fitsinu.hel.curve('dnorm(x,4,59)', 'N(4,59)', xlim=c(-180, 180))

### Chisq
fitsinu.hel.curve('dchisq(x, 14)', expression({chi^2} (14)), xlim=c(1,33))

### t
fitsinu.hel.curve('dt(x,7)', 't(7)')

# Exponential
fitsinu.hel.curve('dexp(x, 3.2)', 'Exp(3.2)', xlim=c(0,1.2))

### GAMMA
fitsinu.hel.curve('dgamma(x,3,6)', 'Gamma(3,6)', xlim=c(0, 1.8))

### BETA
fitsinu.hel.curve('dbeta(x,3,6)', 'Beta(3,6)', xlim=c(0, 0.8))
