fit.Fx.loss = function(F, x, loss, support_x = c(min(x), max(x))){
  F = f_family$F
  rangepars = f_family$rangepars
  support = f_family$support
  loss_pars = function(pars){
    f_pars = function(x) f(x, pars)
    support = support_intersection(support_f(pars), support_g)
    loss(f_pars, g, support)
  }
}


fit.fg.loss <- function(f_family, g, loss, support_g=c(-Inf, Inf), ...){
  #' Fitting a density family f on another density g
  #' @description
  #' Customizable loss function. Density g must be completely specified, e.g. dnorm(x, 3,2). Both densities must be functions of x. f should be a function of pars also
  #' @param loss Loss function to be *minimized*
  f = f_family[[1]]
  rangepars = f_family[[2]]
  support_f = f_family[[3]]
  loss_pars = function(pars){
    f_pars = function(x) f(x, pars)
    support = support_intersection(support_f(pars), support_g)
    loss(f_pars, g, support)
  }
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B')
  return(optim1)
}