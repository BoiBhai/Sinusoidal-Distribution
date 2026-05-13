fitsinu.msm <- function(msm.target, flip = FALSE, init.par = c(1, 1), lower.par = c(0.001, 0.001), upper.par = c(1000, 1000)) {
  mean.t <- msm.target[1]; var.t <- msm.target[2]; gamma.t <- msm.target[3]; kappa.t <- msm.target[4]
  
  costFn <- function(pars) {
    omega <- pars[1]; chi <- pars[2]
    skLoss <- sinu.skew(0, 1, omega, chi, flip) - gamma.t
    kuLoss <- sinu.kurt(0, 1, omega, chi, flip) - kappa.t
    sum(abs(skLoss), abs(kuLoss))
  }
  
  optim1 <- optim(init.par, fn = costFn, lower = lower.par, upper = upper.par, method = "L-BFGS-B")
  
  omega_optim <- optim1$par[1]
  chi_optim <- optim1$par[2]
  delta_cor <- sqrt(var.t / sinu.var(0, 1, omega_optim, chi_optim, flip))
  alpha_cor <- mean.t - sinu.mean(0, delta_cor, omega_optim, chi_optim, flip)
  
  optim1$par <- c(alpha_cor, delta_cor, omega_optim, chi_optim)
  
  return(optim1)
}

#############################################################################
# LOSSES
#############################################################################

L2_msm_loss = function(f_mom, g_mom) {
  sum((f_mom-g_mom)^2)
}

#############################################################################
# MAIN FUNCTION AND METHODS
#############################################################################

fit.m1m2.loss = function(f_family, mg, loss){
  if (mf=='') mf = f_family$msm
  rangepars = f_family$rangepars
  loss_pars = function(pars) loss(mf(pars), mg)
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B')
  return(optim1)
}

fit.m1m2.L2 = function(f_family, mg, loss) fit.mfmg.loss(f_family, mg, L2_msm_loss) 


#############################################################################
# CALLS
#############################################################################
