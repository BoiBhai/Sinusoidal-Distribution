fit.FGn.loss = function(f_family, Gn, loss, support_x = c(min(x), max(x))){
  F_cdf = f_family$cdf
  F_rangepars = f_family$rangepars
  loss_pars = function(pars){
    F_pars = function(x) F_cdf(x, pars)
    return(loss(F_cdf, Gn))
  }
  optim1 = optim(rangepars_f$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B')
  return(optim1)
}


fit.FGn.chisq = function(f_family, Gn, support_x = c(min(x), max(x))) fit.Fx.loss(family_f, x, loss=chisq_dist, support_x)
fit.Fx.L2_Fgn_dist = function(family_f, Gn, support_x = c(min(x), max(x))) fit.Fx.loss(family_f, x, loss=L2_FGn_dist, support_x)

fit.FGn.chisq(family_sinu, ecdf)

ecdf1 = ecdf(rnorm(100))
knots(ecdf1)

############################################################

L2_FGn_dist = function(F, Gn, knots=knots(Gn)){  # specify knots manually to prevent recalculation each time
  FminusGn2 = function(x) (F(x) - Gn(x))^2
  dist = sum(sapply(knots, FminusGn2))
  return(dist)
}

KS_dist = function(F, Gn, knots=knots(Gn)){
  absFminusGn = function(x) abs(F(x) - Gn(x))
  dist = max(sapply(knots, absFminusGn))
  return(dist)
}
