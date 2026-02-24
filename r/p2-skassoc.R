
sinu.kurt.sk = Vectorize(function(s, k) sinu.kurt(s=s, k=k), vectorize.args=c('k','s'))
sinu.skew.sk = Vectorize(function(s, k) sinu.skew(s=s, k=k), vectorize.args=c('k','s'))

#par(mar = c(4,4,0,0))
dev.new(width = 8, height = 6, noRStudioGD = TRUE)
par(mar=c(4,4,0,0))

s.vec=c(seq(0.1, 0.3, by=0.05), 1, seq(5, 40, by=10))
n=length(s.vec)
curve(0*x, xlim=c(0,40), ylim=c(-1.3, 20), xlab='k', ylab='Kurtosis')
i=0
i.vec=c()
for (s in s.vec){
  i=i+1; print(i)
  curve(sinu.kurt.sk(s=s, k=x), col=i,#rgb((n-i)/n, i/n, 0),
        lwd=10-i/1.5, add=T)
i.vec = c(i.vec, i)
}
abline(h=0)
legend('topleft', legend=paste0(s.vec), col=i.vec, #rgb((n-i.vec)/n, i.vec/n, 0),
       lwd=10-i.vec/1.5, title='s')
dev.copy2pdf(file='2-kurtvsk.pdf')


k.vec=c(seq(0.1, 0.3, by=0.05), 1:10)
n=length(k.vec)
curve(0*x, xlim=c(0,50), ylim=c(-1.3,0.6), xlab='s', ylab='Skewness')
i=0
i.vec=c()
for (k in k.vec){
  i=i+1; print(i)
  curve(sinu.skew.sk(k=k, s=x), col=i,#rgb((n-i)/n, i/n, 0),
        lwd=2, add=T)
  i.vec = c(i.vec, i)
}
abline(h=0)
legend('topright', legend=paste0(k.vec), fill=i.vec, title='k')
dev.copy2pdf(file='2-skewvss.pdf')


dev.off()