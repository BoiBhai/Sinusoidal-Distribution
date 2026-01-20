fit.mfmg.loss = function(f_family, mg, loss){
  if (mf=='') mf = f_family$msm
  rangepars = f_family$rangepars
  loss_pars = function(pars) loss(mf(pars), mg)
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B')
  return(optim1)
}

fit.mfmg.L2 = function(f_family, mg, loss) fit.mfmg.loss(f_family, mg, L2_msm_loss) 
