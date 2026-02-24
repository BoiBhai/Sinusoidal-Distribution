dev.new(width = 8, height = 6, noRStudioGD = TRUE)

par(mar=c(2,4,0,0))

curve(0*x, col='white', ylim=c(0,8), ylab='PDF')
curve(dsinustd(x, 0.1,2), col='brown', add=T)
curve(dsinustd(x, 0.1,1), col='red', add=T, lwd=2, lty=2)
curve(dsinustd(x, 0.1,0.5), col='orange', add=T, lwd=2)
curve(dsinustd(x, 3,8), col='blue', add=T)
curve(dsinustd(x, 3,1), col='darkgreen', add=T, lwd=2, lty=2)
curve(dsinustd(x, 3,0.5), col='green', add=T, lwd=2)
curve(dsinustd(x, 1,1), col='black', add=T, lwd=2, lty=3)
legend('topleft', legend=c('Sinu(0.1, 2)',
                       'Sinu(0.1, 1)',
                       'Sinu(0.1, 0.5)'),
       col=c('brown', 'red', 'orange'),
       lwd=c(1,2,2), lty=c(1,2,1))

legend('topright', legend=c('Sinu(3, 8)',
                       'Sinu(3, 1)',
                       'Sinu(3, 0.5)'),
       col=c('blue', 'darkgreen', 'green'),
       lwd=c(1,2,2), lty=c(1,2,1))
legend('top', legend='Sinu(1, 1)', col='black',lwd=2, lty=3)
dev.copy2pdf(file='2-pdf.pdf')





curve(x*0+1, col='grey', ylim=c(0,1.3), ylab='CDF')
curve(psinustd(x, 0.1,2), col='brown', add=T)
curve(psinustd(x, 0.1,1), col='red', add=T, lwd=2, lty=2)
curve(psinustd(x, 0.1,0.5), col='orange', add=T, lwd=2)
curve(psinustd(x, 3,8), col='blue', add=T)
curve(psinustd(x, 3,1), col='darkgreen', add=T, lwd=2, lty=2)
curve(psinustd(x, 3,0.5), col='green', add=T, lwd=2)
curve(psinustd(x, 1,1), col='black', add=T, lwd=2, lty=3)
legend('topleft', legend=c('Sinu(0.1, 2)',
                           'Sinu(0.1, 1)',
                           'Sinu(0.1, 0.5)'),
       col=c('brown', 'red', 'orange'),
       lwd=c(1,2,2), lty=c(1,2,1))

legend('bottomright', legend=c('Sinu(3, 8)',
                            'Sinu(3, 1)',
                            'Sinu(3, 0.5)'),
       col=c('blue', 'darkgreen', 'green'),
       lwd=c(1,2,2), lty=c(1,2,1))
legend('topright', legend='Sinu(1, 1)', col='black',lwd=2, lty=3)
dev.copy2pdf(file='2-cdf.pdf')




curve(x*0, col='white', ylim=c(0,20), ylab='Hazard')
curve(hsinustd(x, 0.1,2), col='brown', add=T)
curve(hsinustd(x, 0.1,1), col='red', add=T, lwd=2, lty=2)
curve(hsinustd(x, 0.1,0.5), col='orange', add=T, lwd=2)
curve(hsinustd(x, 3,8), col='blue', add=T)
curve(hsinustd(x, 3,1), col='darkgreen', add=T, lwd=2, lty=2)
curve(hsinustd(x, 3,0.5), col='green', add=T, lwd=2)
curve(hsinustd(x, 1,1), col='black', add=T, lwd=2, lty=3)
legend('topleft', legend=c('Sinu(0.1, 2)',
                           'Sinu(0.1, 1)',
                           'Sinu(0.1, 0.5)'),
       col=c('brown', 'red', 'orange'),
       lwd=c(1,2,2), lty=c(1,2,1))

legend('left', legend=c('Sinu(3, 8)',
                               'Sinu(3, 1)',
                               'Sinu(3, 0.5)'),
       col=c('blue', 'darkgreen', 'green'),
       lwd=c(1,2,2), lty=c(1,2,1))
legend('top', legend='Sinu(1, 1)', col='black',lwd=2, lty=3)
dev.copy2pdf(file='2-hf.pdf')





curve(0*x, col='white', xlim=c(0,1.05), ylim=c(0,20), ylab='Density-Quantile')
curve(dqsinustd(x, 0.1,2), col='brown', add=T)
curve(dqsinustd(x, 0.1,1), col='red', add=T, lwd=2, lty=2)
curve(dqsinustd(x, 0.1,0.5), col='orange', add=T, lwd=2)
curve(dqsinustd(x, 3,8), col='blue', add=T)
curve(dqsinustd(x, 3,1), col='darkgreen', add=T, lwd=2, lty=2)
curve(dqsinustd(x, 3,0.5), col='green', add=T, lwd=2)
curve(dqsinustd(x, 1,1), col='black', add=T, lwd=2, lty=3)
legend('topleft', legend=c('Sinu(0.1, 2)',
                           'Sinu(0.1, 1)',
                           'Sinu(0.1, 0.5)'),
       col=c('brown', 'red', 'orange'),
       lwd=c(1,2,2), lty=c(1,2,1))

legend('topright', legend=c('Sinu(3, 8)',
                            'Sinu(3, 1)',
                            'Sinu(3, 0.5)'),
       col=c('blue', 'darkgreen', 'green'),
       lwd=c(1,2,2), lty=c(1,2,1))
legend('top', legend='Sinu(1, 1)', col='black',lwd=2, lty=3)
dev.copy2pdf(file='2-dqf.pdf')
dev.off()