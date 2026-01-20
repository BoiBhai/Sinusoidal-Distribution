source('1-sinudist.R')
source('2.0-losses.R')
source('2.0-families.R')

# Calculating intersection of 2 supports for optimized metric calculation
support_intersection = function(support1, support2){
  out = c(max(support1[1], support2[1]), min(support1[2], support2[2]))
  if (out[1]>out[2]) return(NULL)
  return(out)
}


fit.fg.loss <- function(f_family, g, loss, support_g=c(-Inf, Inf), ...){
  #' Fitting a density family f on another density g
  #' @description
    #' Customizable loss function. Density g must be completely specified, e.g. dnorm(x, 3,2). Both densities must be functions of x. f should be a function of pars also
  #' @param loss Loss function to be *minimized*
  f = f_family$f
  rangepars = f_family$rangepars
  support_f = f_family$support
  loss_pars = function(pars){
    f_pars = function(x) f(x, pars)
    support = support_intersection(support_f(pars), support_g)
    return(loss(f_pars, g, support))
  }
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B')
  return(optim1)
}

fit.fg.Hel = function(f_family, g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=f_family, g=g, loss=Hel_dist, support_g=support_g)
fit.fg.KL = function(f_family, g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family, g, loss=KL_loss, support_g)
fit.fg.DPD = function(f_family, g, a, support_g=c(-Inf, Inf))fit.fg.loss(f_family, g, loss=function(f,g,support) DPD_dist(f,g,a,support), support_g)
