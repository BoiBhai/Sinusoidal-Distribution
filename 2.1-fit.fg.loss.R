source('1-sinudist.R')

# Calculating intersection of 2 supports for optimized metric calculation
support_intersection = function(support1, support2){
  out = c(max(support1[1], support2[1]), min(support1[2], support2[2]))
  if (out[1]>out[2]) return(NULL)
  return(out)
}

# N(mu, theta)
family_norm = list(f_norm, rangepars_norm, support_norm)
support_norm = function(pars) c(-Inf, Inf)
f_norm = function(x, pars) dnorm(x, pars[1], pars[2])
rangepars_norm = list(init=c(0,1),
                      lower=c(-100, 0.001),
                      upper=c(100, 100))

# Location (Shifted) gamma(l, n, lambda)
family_lgamma = list(f_lgamma, rangepars_lgamma, support_lgamma)
f_lgamma = function(x, pars) dgamma(x-pars[1], pars[2], pars[3])
rangepars_lgamma = list(init=c(0,1,1),
                        lower=c(-100, 0.001, 0.001),
                        upper=c(100, 100, 100))
support_lgamma = function(pars) c(pars[1], Inf)

# Location Scale Beta(l, d, a,b)
family_lsbeta = list(f_lsbeta, rangepars_lsbeta, support_lsbeta)
f_lsbeta = function(x, pars) 1/pars[2]*dbeta((x-pars[1])/pars[2], pars[3], pars[4])
rangepars_lsbeta = list(init=c(0,1,1,1),
                        lower=c(-100, 0.001, 0.001, 0.001),
                        upper=c(100, 100, 100, 100))
support_lsbeta = function(pars) c(pars[1], pars[1]+pars[2])

# Sinu(a,d,s,k)
f = function(x, pars) dsinu(x, pars[1], pars[2], pars[3], pars[4])
rangepars = list(init=c(0,1,1,1),
                      lower=c(-100, 0.001, 0.001, 0.001),
                      upper=c(100, 100, 100, 100))
support = function(pars) c(pars[1], pars[1]+pars[2])
family_sinu = list(f, rangepars, support)

# Symmetric Sinu(-d/2,d,1,k)
f = function(x, pars) dsinu(x, -pars[1]/2, pars[1], 1, pars[2])
rangepars = list(init=c(1,1),
                 lower=c(0.001, 0.001),
                 upper=c(100,100))
support = function(pars) c(-pars[1]/2, pars[1]/2)
family_sinuSymAbt0 = list(f, rangepars, support)



fit.fg.loss <- function(f_family, g, loss, support_g=c(-Inf, Inf), ...){
  #' Fitting a density family f on another density g
  #' @description
    #' Customizable loss function. Density g must be completely specified, e.g. dnorm(x, 3,2). Both densities must be functions of x. f should be a function of pars also
  #' @param loss Loss function to be *minimized*
  f = f_family$f
  rangepars = f_family$rangepars
  support_f = f_family$support
  f_pars = function(x) f(x, pars)
  support = support_intersection(support_f(pars), support_g)
  loss_pars = function(pars){
    loss(f_pars, g, support)
  }
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B')
  return(optim1)
}

fit.fg.Hel = function(f_family, g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=f_family, g=g, loss=Hel_dist, support_g=support_g)
fit.fg.KL = function(f_family, g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family, g, loss=KL_loss, support_g)
fit.fg.DPD = function(f_family, g, a, support_g=c(-Inf, Inf))fit.fg.loss(f_family, g, loss=function(f,g,support) DPD_dist(f,g,a,support), support_g)
