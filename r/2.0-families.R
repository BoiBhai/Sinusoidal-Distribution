# N(mu, theta)
pdf = function(x, pars) dnorm(x, pars[1], pars[2])
cdf = function(x, pars) pnorm(x, pars[1], pars[2])
dqf = function(p) dnorm(qnorm(p))
dqf.area = integrate(dqf, lower=0, upper=1)$value
msm = function(pars) c(pars[1], pars[2]^2, 0,0)
support = function(pars) c(-Inf, Inf)
rangepars = list(init=c(0,1),
                lower=c(-100, 0.001),
                upper=c(100, 100))
family_norm = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)

# Location (Shifted) gamma(l, n, lambda)
pdf = function(x, pars) dgamma(x-pars[1], pars[2], pars[3])
cdf = function(x, pars) dgamma(x-pars[1], pars[2], pars[3])
# dqf = function(x, pars) dgamma(qgamma(p)-pars[1], pars[2], pars[3])
dqf.area = integrate(dqf, lower=0, upper=1)$value
#msm = c(l*n*lambda) # needs work
support = function(pars) c(pars[1], Inf)
rangepars = list(init=c(0,1,1),
                        lower=c(-100, 0.001, 0.001),
                        upper=c(100, 100, 100))
family_lgamma = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)

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
family_lsbeta = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)

# Sinu(a,d,s,k)
pdf = function(x, pars) dsinu(x, pars[1], pars[2], pars[3], pars[4])
cdf = function(x, pars) psinu(x, pars[1], pars[2], pars[3], pars[4])
dqf = function(p, pars) dsinustd(qsinustd(p, pars[3], pars[4]), pars[3], pars[4])
dqf.area = function(pars) integrate(function(x) dqf(x, pars), lower=0, upper=1)$value
msm = function(pars) sinu.msm(pars[1], pars[2], pars[3], pars[4])
support = function(pars) c(pars[1], pars[1]+pars[2])
rangepars = list(init=c(0,0.1,0.1,0.1),
                 lower=c(-10^3, 0.1^2, 0.1^2, 0.1^2),
                 upper=c(10^3, 2*10^3, 10^2, 10^2))
family_sinu = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)

# Sinu(s,k)
pdf = function(x, pars) dsinustd(x, pars[1], pars[2])
cdf = function(x, pars) psinustd(x, pars[1], pars[2])
dqf = function(p, pars) dsinustd(qsinustd(p, pars[3], pars[4]), pars[3], pars[4])
dqf.area = function(pars) integrate(function(x) dqf(x, pars), lower=0, upper=1)$value
msm = function(pars) sinu.msm(0,1, pars[3], pars[4])
support = function(pars) c(0,1)
rangepars = list(init=c(0.1,0.1),
                 lower=c(0.1^2, 0.1^2),
                 upper=c(10^2, 10^2))
family_sinustd = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)

# Sinu(-d/2,d,1,k) Symmetric about 0
pdf = function(x, pars) dsinu(x, -pars[1]/2, pars[1], 1, pars[2])
cdf = function(X, pars) psinu(x, -pars[1]/2, pars[1], 1, pars[2])
dqf = function(p, pars) dsinustd(qsinustd(p, 1, pars[2]), 1, pars[2])
dqf.area = function(pars) integrate(dqf, lower=0, upper=1)$value
msm = function(pars) sinu.msm(pars[1], pars[2], pars[3], pars[4])
support = function(pars) c(-pars[1]/2, pars[1]/2)
rangepars = list(init=c(1,1),
                 lower=c(0.1^5, 0.1^5),
                 upper=c(10^5, 10^5))
family_sinuSymAbt0 = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)