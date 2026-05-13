
### Usual fg fit strategy

pdf = function(x, pars) dsinu(x, 0,2, pars[1], pars[2])
cdf = function(x, pars) psinu(x, 0,2, pars[1], pars[2])
support = function(pars) c(0,2)
rangepars = list(init=c(1,1),
                 lower=c(0.1^2, 0.1^2),
                 upper=c(10^3, 10^3))
family_sinu02 = list(pdf=pdf, cdf=cdf, dqf=dqf, dqf.area=dqf.area, msm=msm, support=support, rangepars=rangepars)


for (s in 1:3) {
  for (k in 1:3) {
    optim1 = fit.fg.Hel(family_sinu02, function(x) f1f2(x,s,k), support_g=c(0,2))
    par1 = optim1$par
    par1
  }
}