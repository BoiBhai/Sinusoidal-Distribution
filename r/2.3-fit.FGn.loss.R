#############################################################################
# LOSSES
#############################################################################

L2_FGn_dist = function(F1, Gn, knots=knots(Gn)){  # specify knots manually to prevent recalculation each time
  FminusGn2 = function(x) (F1(x) - Gn(x))^2
  dist = mean(sapply(knots, FminusGn2))
  return(dist)
}

KS_dist = function(F, Gn, knots=knots(Gn)){
  absFminusGn = function(x) abs(F(x) - Gn(x))
  dist = max(sapply(knots, absFminusGn))
  return(dist)
}

#############################################################################
# MAIN FUNCTION AND METHODS
#############################################################################

support_union = function(support1, support2){
  out = c(min(support1[1], support2[1]), max(support1[2], support2[2]))
  if (out[1]>out[2]) return(NULL)
  return(out)
}

fit.FGn.loss = function(f_family, Gn, loss, knots=knots(Gn)){
  # Gn must have associated knots, as in ECDFs, for example.
  F1 = f_family$cdf
  rangepars = f_family$rangepars
  a.out = min(knots)
  d.out = max(knots)-min(knots)
  knots_norm = (knots - a.out)/d.out
  Gn_norm = function(p) Gn(a.out + p*d.out)
  loss_pars = function(pars){
    F_pars = function(x) F1(x, pars)
    return(loss(F_pars, Gn_norm, knots=knots_norm))
  }
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B', control=list())
  optim1$par = c(a.out, d.out, optim1$par)
  return(optim1)
}


fit.FGn.chisq = function(f_family, Gn, knots) fit.FGn.loss(f_family, Gn, loss=chisq_dist, knots)
fit.FGn.L2 = function(f_family, Gn, knots) fit.FGn.loss(f_family, Gn, loss=L2_FGn_dist, knots)

#############################################################################
# CALLS
#############################################################################

fit.sinuGn.L2 = function(Gn, knots) fit.FGn.L2(family_sinustd, Gn, knots)
