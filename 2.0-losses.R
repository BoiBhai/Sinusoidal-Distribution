source('1-sinudist.R')

################################################################################
#' PMF-PMF losses
#' Won't be needed for our purposes

Chi2_dist = function(p, q, w, masspoints) {
  summand = function(x) (p(x)-q(x))^2/w
  sum = 0
  for (x in masspoints) sum = sum+summand(x)
  return(sum)
}

symmChi2_dist = function(p, q, masspoints) {
  Chi2_dist(p,q, w=function(x) 1/2*(p(x)+q(x)), masspoints)
}


#' PDF-PDF losses

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

################################################################################

#' MSM-MSM Losses

L2_msm_loss = function(f_mom, g_mom) {
  sum((f_mom-g_mom)^2)
}


################################################################################

#' CDF-CDF Losses

L2_FG_dist = function(F, G, support=c(-Inf, Inf)){
  FminusG2 = function(x) (F(x)-G(x))^2
  integrate(FminusG2, support[1], support[2])$value
}

Kol_dist = function(F, G, support=c(-Inf, Inf)){
  absFminusG = function(x) abs(F(x)-G(x))
  #gridSearch??
}

#' CDF-ECDF Losses

