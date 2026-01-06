source('2.1-fit.fg.loss.R')
curve.fg = function(f, g, xlim=c(-1,1), dists=c(Hel_dist), legend='') {
  curve(g, xlim=xlim, ylab='Density')
  curve(f, add=T, col='red')
  vals <- sapply(
    1:length(dists),
    function(i) {
      val <- dists[[i]](f, g)
      signif(val, 6)
    })
  if(legend=='') legend=c(substitute(f), substitute(g))
  legend('topright', legend=legend, col=c('red', 'black'), lwd=1)
  legend('topleft', legend=vals)
  return(vals)
}