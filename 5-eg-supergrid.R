dist.vec = c('dnorm(x, 3,1)',
              'dexp(x, 4)',
              'dgamma(x, 4,3.4)',
              'dbeta(x, 6,7)',
              'dt(x, 3)',
              'dchisq(x,4)')

alpha.vec = c(0.2,0.4,0.6,0.8,1.0)

supergrid = function(dist.vec) {
  par(mfrow=c(length(alpha.vec)+1, length(dist.vec)), cex=0.6)
  for (dist in dist.vec) {
    fitsinu.pdf.curve(dist)
    for (alpha in alpha.vec) {
      fitsinu.dpd.curve(alpha, dist)
    }
  }
}

supergrid(dist.vec)

moment.df = function(dist.vec) {
  df = data.frame(matrix(0, nrow=length(dist.vec), ncol=length(alpha.vec)+1))
  for (dist in dist.vec) {
    i = match(dist, dist.vec)
    dist.p = parse(text=dist)
    dist = function(x) eval(dist.p)
    pars = fitsinu.pdf(dist)$par
    msm_hel = paste(format(sinu.msm(pars[1], pars[2], pars[3], pars[4]), digits=1), collapse=', ')
    df[i,1] = msm_hel
    for (alpha in alpha.vec) {
      j = match(alpha, alpha.vec)
      print(c(i,j))
      pars = fitsinu.dpd(alpha, dist)$par
      msm_dpd = paste(format(sinu.msm(pars[1], pars[2], pars[3], pars[4]), digits=1), collapse=', ')
      df[i, j+1] = msm_dpd
    }
  }
  rownames(df) = dist.vec
  colnames(df) = c('H', alpha.vec)
  return(df)
}
  
momentdf = moment.df(dist.vec)

partable = function(dist.vec) {
  df = data.frame(matrix(0, nrow=length(dist.vec), ncol=length(alpha.vec)+1))
  for (dist in dist.vec) {
    i = match(dist, dist.vec)
    dist.p = parse(text=dist)
    dist = function(x) eval(dist.p)
    pars = fitsinu.pdf(dist)$par
    par_hel = paste(pars, collapse=',')
    df[i,1] = par_hel
    for (alpha in alpha.vec) {
      j = match(alpha, alpha.vec)
      print(c(i,j))
      pars = fitsinu.dpd(alpha, dist)$par
      par_dpd = paste(pars, collapse=',')
      df[i, j+1] = par_dpd
    }
  }
  rownames(df) = dist.vec
  colnames(df) = c('H', alpha.vec)
  return(df)
}

partable = partable(dist.vec)

sinu.msm.par = function(pars) {
  sinu.msm(pars[1], pars[2], pars[3], pars[4])
}