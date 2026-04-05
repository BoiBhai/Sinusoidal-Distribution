library(numDeriv)

sumdens = Vectorize(function(x,s,k) {
  integrand = function(t) dsinustd(t,s,k)*dsinustd(x-t, s,k)
  integrate(integrand, lower=0, upper=1)$value
})
curve(sumdens(x,0.2,0.3), xlim=c(0,2))
sumdens.deriv = function(x, s,k) grad(function(t) sumdens(t,s,k), x)

sumdens.mode = function (s,k) uniroot(function(x) sumdens.deriv(x,s,k), c(0.01,1.99))$root
sumdens.modaldens = function(s,k) sumdens(sumdens.mode(s,k), s,k)

# Mode fitting strategy
sumdens.mode(3,2)
sumdens.modaldens(3,2)
s.fit = function(s,k) 1/(1-log(sumdens.mode(s,k))/log(2))
k.fit = function(s,k) uniroot(function(t) sinuarea(s.fit(s,k),t) - 1/2*1/sumdens.modaldens(s,k), c(0.1,10))$root

s1 = 3; k1 = 1
curve(sumdens(x,s1,k1), xlim=c(0,2))
s.fit(s1,k1)
k.fit(s1,k1)
curve(dsinu(x, 0,2, s.fit(s1,k1), k.fit(s1,k1)), col='red', add=T)

################################################################################

# Sinu(0,2,s,k) family
pdf = function(x, pars) dsinu(x, 0,2, pars[1], pars[2])
cdf = function(X, pars) psinu(x, 0,2, pars[1], pars[2])
#dqf = function(p, pars) dsinustd(qsinustd(p, 1, pars[2]), 1, pars[2])
#dqf.area = function(pars) integrate(dqf, lower=0, upper=1)$value
#msm = function(pars) sinu.msm(pars[1], pars[2], pars[3], pars[4])
mode = function(pars) 2*2^(-1/pars[1])
modedens = function(pars) 1/(d*A(s,k))
support = function(pars) c(0,2)
rangepars = list(init=c(1,1),
                 lower=c(0.1^5, 0.1^5),
                 upper=c(10^5, 10^5))
family_sinu02 = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, mode=mode, modedens=modedens, support=support, rangepars=rangepars)


for (s in 1:10) {
  for (k in 1:10) {
    optim1 = fit.fg.Hel(family_sinu02, function(x) sumdens(x,s,k), support_g=c(0,2))
    par1 = optim1$par
    par
  }
}


