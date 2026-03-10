dev.new(width = 8, height = 6, noRStudioGD = TRUE)

par(mfrow=c(2,3))
par(mar=c(2,4,0,0))

curve(dlnorm(x), xlim=c(0,5))
curve(dsinu(x, 0.02827762, 1000, 0.08566970, 35.78972), add=T, col='red')

curve(dlogis(x), xlim=c(-3,3))
curve(dsinu(x, -80.275639, 102.222513, 2.882431, 100), add=T, col='red')

curve(dexp(x), xlim=c(0,5))
curve(dsinu(x, 0.0005708485, 6.5585684475, 0.0621246472, 1.6766166256), add=T, col='red')

curve(dbeta(x,1,1), xlim=c(-1,2), ylim=c(0,1.2))
curve(dsinu(x, -0.0007780335, 1.0012066335, 0.1004892336, 0.12190192670), add=T, col='red')

curve(dcauchy(x), xlim=c(-3,3))
curve(dsinu(x, -126.211809, 149.303341, 4.141315, 100), add=T, col='red')

curve(dchisq(x,10), xlim=c(0,30))
curve(dsinu(x, -0.3201950, 97.1337699, 0.2815837, 22.0271292), add=T, col='red')

dev.copy2pdf(file='3-pdfonpdf.pdf')
dev.off()
