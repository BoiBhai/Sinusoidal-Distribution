# N(mu, theta)
pdf = function(x, pars) dnorm(x, pars[1], pars[2])
cdf = function(x, pars) pnorm(x, pars[1], pars[2])
dqf = function(p, pars) dnorm(qnorm(p))
dqf.area = integrate(dqf, lower=0, upper=1)$value
msm = function(pars) c(pars[1], pars[2]^2, 0,0)
support = function(pars) c(-Inf, Inf)
rangepars = list(init=c(0,1),
                      lower=c(-100, 0.001),
                      upper=c(100, 100))
family_norm = list(pdf, cdf, dqf, dqf.area, msm, support, rangepars)

# Location (Shifted) gamma(l, n, lambda)
pdf = function(x, pars) dgamma(x-pars[1], pars[2], pars[3])
cdf = function(x, pars) dgamma(x-pars[1], pars[2], pars[3])
# dqf = function(x, pars) dgamma(qgamma(p)-pars[1], pars[2], pars[3])
dqf.area = integrate(dqf, lower=0, upper=1)$value
msm = c(l*n*lambda) # needs work
support = function(pars) c(pars[1], Inf)
rangepars = list(init=c(0,1,1),
                        lower=c(-100, 0.001, 0.001),
                        upper=c(100, 100, 100))
family_lgamma = list(pdf, cdf, dqf, dqf.area, msm, support, rangepars)

# Location Scale Beta(l, d, a,b)
pdf = function(x, pars) 1/pars[2]*dbeta((x-pars[1])/pars[2], pars[3], pars[4])
cdf = function(x, pars) pbeta((x-pars[1])/pars[2], pars[3], pars[4])
# dqf = 
dqf.area = integrate(dqf, lower=0, upper=1)$value
msm = function(pars) c(l+d*a/(a+b), d^2*a*b/((a+b)^2*(a+b+1)), 2*(b-a)*sqrt(a+b+1)/((a+b+2)*sqrt(a*b)), 6*((a-b)^2*(a+b+1) - a*b*(a+b+2))/(a*b*(a+b+2)*(a+b+3)))
rangepars_lsbeta = list(init=c(0,1,1,1),
                        lower=c(-100, 0.001, 0.001, 0.001),
                        upper=c(100, 100, 100, 100))
support_lsbeta = function(pars) c(pars[1], pars[1]+pars[2])
family_lsbeta = list(pdf, cdf, dqf, dqf.area, msm, support, rangepars)

# Sinu(a,d,s,k)
pdf = function(x, pars) dsinu(x, pars[1], pars[2], pars[3], pars[4])
cdf = function(x, pars) psinu(x, pars[1], pars[2], pars[3], pars[4])
dqf = function(p, pars) dsinu(qsinu(p, pars[1], pars[2], pars[3], pars[4]), pars[1], pars[2], pars[3], pars[4])
dqf.area = integrate(dqf, lower=0, upper=1)$value
msm = function(pars) sinu.msm(pars[1], pars[2], pars[3], pars[4])
support = function(pars) c(pars[1], pars[1]+pars[2])
rangepars = list(init=c(0,1,1,1),
                 lower=c(-100, 0.001, 0.001, 0.001),
                 upper=c(100, 100, 100, 100))
family_sinu = list(pdf, cdf, dqf, dqf.area, msm, support, rangepars)


# Sinu(-d/2,d,1,k) Symmetric about 0
pdf = function(x, pars) dsinu(x, -pars[1]/2, pars[1], 1, pars[2])
cdf = function(x, pars) psinu(x, -pars[1]/2, pars[1], 1, pars[2])
dqf = function(p, pars) dsinu(psinu(p, -pars[1]/2, pars[1], 1, pars[2]), -pars[1]/2, pars[1], 1, pars[2])
dqf.area = integrate(dqf, lower=0, upper=1)$value
msm = function(pars) sinu.msm(pars[1], pars[2], pars[3], pars[4])
support = function(pars) c(-pars[1]/2, pars[1]/2)
rangepars = list(init=c(1,1),
                 lower=c(0.001, 0.001),
                 upper=c(100,100))
family_sinuSymAbt0 = list(pdf, cdf, dqf, dqf.area, msm, support, rangepars)