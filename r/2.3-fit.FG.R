#############################################################################
# LOSSES
#############################################################################

L2_FG_dist = function(F1, G, support=c(-Inf, Inf)){
  FminusG2 = function(x) (F1(x)-G(x))^2
  integrate(FminusG2, lower=support[1], upper=support[2])$value
}

Kol_dist = function(F1, G, support=c(-Inf, Inf)){
  absFminusG = function(x) abs(F1(x)-G(x))
  # gridSearch??
}

#############################################################################
# MAIN FUNCTION AND METHODS
#############################################################################

support_union = function(support1, support2){
  out = c(min(support1[1], support2[1]), max(support1[2], support2[2]))
  if (out[1]>out[2]) return(NULL)
  return(out)
}

fit.FG.loss <- function(f_family, G, loss, support_g=c(-Inf, Inf), ...){
  F1 = f_family$cdf
  rangepars = f_family$rangepars
  support_f = f_family$support
  loss_pars = function(pars){
    F_pars = function(x) F1(x, pars)
    support = support_union(support_f(pars), support_g)
    return(loss(F_pars, G, support))
  }
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B', control=list(maxit=1000))
  return(optim1)
}

fit.FG.L2 = function(f_family, G, support_g=c(-Inf, Inf)) fit.FG.loss(f_family=f_family, G=G, loss=L2_FG_dist, support_g=support_g)

#############################################################################
# CALLS
#############################################################################

fit.sinuG.L2 = function(G, support_g=c(-Inf, Inf)) fit.FG.L2(f_family=family_sinu, G=G, support_g=support_g)


fit.sinuG.L2(function(x) pbeta(x, 3,4))
#curve.fg(function(x) psinu(x,0.4531058,0.0100000,0.0100000, 0.0100000), pnorm)
