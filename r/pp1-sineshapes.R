dev.new(width=25, height=5, noRStudioGD = TRUE)
par(mfrow=c(1,5), mar=c(2,2,0,0))
for (k in c(0.5,1,2,3,4)){
  curve((sin(pi*x))^k)
}
dev.copy2pdf(file='p1-sinek.pdf')

par(mfrow=c(1,5), mar=c(2,2,0,0))
for (s in c(0.1,0.5,1,2,3)){
  curve((sin(pi*x^s)))
}
dev.copy2pdf(file='p1-sines.pdf')
dev.off()

