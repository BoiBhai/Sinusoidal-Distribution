dev.new(width = 8, height = 6, noRStudioGD = TRUE)
par(mar=c(2,4,0,0))
curve(dnorm(x), xlim=c(-3,3), lwd=3, ylim=c(0,0.5), ylab='Density')
kseq = c(1,2,3,5,10,20,100)
n = length(kseq)
i=0
for (k in kseq){
  i=i+1
  d = pi * sqrt(k)
  curve(dsinu(x, -d/2, d, 1, k), add=T, col=rgb(1-i/n, i/n, 0), ylab=density)
}
legend('topright', legend=paste0('k=',kseq), lwd=1, col=rgb(1-1:n/n, 1:n/n,0))
legend('topleft', legend='Normal (0,1)', col='black', lwd=3)

dev.copy2pdf(file='2-tendingnormal.pdf')
dev.off()