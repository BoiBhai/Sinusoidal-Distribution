#############################################################################
# LOSSES
#############################################################################
#' Convention : f adaptive, g target. Matters a lot for asymmetric losses.
#' Loss = 0 when distributions are same, o/w > 0. Loss is to be minimized using optim.
#' Gain = opposite(Loss). It is maximum if 2 distributions are same. In literature, this might be known as affinity, but doesn't matter for all we care.

L2_dist = function(f, g, support=c(-Inf, Inf)){
  fminusg = function(x) (f(x) - g(x))^2
  integrate(fminusg, support[1], support[2])$value
}

Hel_gain = function(f, g, support=c(-Inf, Inf)) {
  #' Hellinger Overlap (Affinity/ Bhattacharya Coefficient)
  #' Returns integral of sqrt(f * g). 
  #' Maximizing this is equivalent to minimizing Hellinger Distance.
  sqrtfg = function(x) sqrt(f(x) * g(x))
  integrate(sqrtfg, support[1], support[2])$value
}

Hel_dist = function(f, g, support=c(-Inf, Inf)) 1 - Hel_gain(f, g, support=support)

KL_loss = function(f, g, support=c(-Inf, Inf)){
  #' Kullback Leibler Loss
  #' Returns nonconstant part of KL Divergence
  flogfg = function(x) - log(g(x))*f(x)
  integrate(flogfg, support[1], support[2])$value
}

KL_dist = function(f, g, support=c(-Inf, Inf)){
  #' Kullback Leibler Loss
  #' Returns nonconstant part of KL Divergence
  flogfg = function(x) log(f(x)/g(x))*f(x)
  integrate(flogfg, support[1], support[2])$value
}

symmKL_dist = function(f, g, support=c(-Inf, Inf)){
  KL_dist(f,g,support) + KL_dist(g,f,support)
}

JS_dist = function(f, g, support=c(-Inf, Inf)){
  #' Jensen-Shannon Loss
  m = function(x) f(x)+g(x)
  1/2*(KL_loss(f,m) + KL_loss(g,m))
  
}

DPD_dist = function(f, g, a, support=c(-Inf, Inf)){
  integrand = function(x) {
    if (a>0) {
      f(x)^(1+a) - (1+1/a)*f(x)^a*g(x) + 1/a*g(x)^(1+a)
    } else if (a==0) {
      g(x) * (log(g(x)) - log(f(x)))
    } else {
      warning('a parameter cannot be <0')
    }
  }
  integrate(integrand, lower=support[1], upper=support[2])$value
}

Breg_loss = function(f, g, support=c(-Inf, Inf)){
  
}

#############################################################################
# MAIN FUNCTION AND METHODS
#############################################################################
support_intersection = function(support1, support2){
  out = c(max(support1[1], support2[1]), min(support1[2], support2[2]))
  if (out[1]>out[2]) return(NULL)
  return(out)
}

fit.fg.loss <- function(f_family, g, loss, support_g=c(-Inf, Inf), ...){
  #' Fitting a density family f on another density g
  #' @description
    #' Customizable loss function. Density g must be completely specified, e.g. dnorm(x, 3,2). Both densities must be functions of x. Further, f should be a function of a single additional argument pars.
  #' @param loss A loss function to be *minimized*
  f = f_family$pdf
  rangepars = f_family$rangepars
  support_f = f_family$support
  loss_pars = function(pars){
    f_pars = function(x) f(x, pars)
    support = support_intersection(support_f(pars), support_g)
    return(loss(f_pars, g, support))
  }
  optim1 = optim(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, method='L-BFGS-B', control=list(maxit=1000))
    #GenSA(rangepars$init, loss_pars, lower=rangepars$lower, upper=rangepars$upper, control=list(maxit=1000))
  return(optim1)
}

fit.fg.Hel = function(f_family, g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=f_family, g=g, loss=Hel_dist, support_g=support_g)
fit.fg.KL = function(f_family, g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family, g, loss=KL_loss, support_g)
fit.fg.DPD = function(f_family, g, a, support_g=c(-Inf, Inf))fit.fg.loss(f_family, g, loss=function(f,g,support) DPD_dist(f,g,a,support), support_g)

#############################################################################
# CALLS
#############################################################################
fit.sinug.Hel = function(g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=family_sinu, g=g, loss=Hel_dist, support_g=support_g)
fit.sinug.KL = function(g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=family_sinu, g, loss=KL_loss, support_g)
fit.sinug.DPD = function(g, a, support_g=c(-Inf, Inf))fit.fg.loss(f_family=family_sinu, g, loss=function(f,g,support) DPD_dist(f,g,a,support), support_g)

fit.sinuSymAbt0g.Hel = function(g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=family_sinuSymAbt0, g=g, loss=Hel_dist, support_g=support_g)
fit.sinuSymAbt0g.KL = function(g, support_g=c(-Inf, Inf)) fit.fg.loss(f_family=family_sinu, g, loss=KL_loss, support_g)
fit.sinuSymAbt0g.DPD = function(g, a, support_g=c(-Inf, Inf))fit.fg.loss(f_family=family_sinu, g, loss=function(f,g,support) DPD_dist(f,g,a,support), support_g)
