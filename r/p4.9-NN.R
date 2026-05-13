dev.new(width = 4, height = 3, noRStudioGD = TRUE)

par(mar=c(2,2,0,0))

### Plot of ACTIVATION FUNCTIONS
N_RES = 1000
for (s in c(0.5,1,3)){
  for (k in c(1,3,5)){
    curve(psinustd(x, s,k))
    title(main=paste0('Sinu(',s,',',k,')'), line = -1)
    upper_crit = which.max(psinustd(1:N_RES/N_RES, s,k)>0.95)/N_RES
    bottom_crit = which.min(psinustd(1:N_RES/N_RES, s,k)<0.05)/N_RES
    upper_arm = round(1 - upper_crit, 3)
    bottom_arm = round(bottom_crit, 3)
    legend('right', legend=paste0('U=', upper_arm), fill='green')
    legend('left', legend=paste0('B=', bottom_arm), fill='red')
    abline(v=upper_crit, col='green')
    abline(v=bottom_crit, col='red')
    dev.copy2pdf(file=paste0('4.9-NN-CDF-Sinu',s,',',k,'.pdf'))
    print(c(upper_arm, bottom_arm))
  }
}

dev.off()


dev.new(width = 8, height = 6, noRStudioGD = TRUE)

### PLOT OF MODEL PERFORMANCES

### DATA
sigm = list(loss = c(0.7046, 0.2300, 0.1666, 0.1292, 0.1025), acc = c(91.88, 94.37, 95.47, 96.26, 96.68), time=28.4)
gelu = list(loss = c(0.3480, 0.1344, 0.0869, 0.0627, 0.0487), acc = c(95.06, 96.79, 97.30, 97.62, 97.73), time=25.4)
swish = list(loss = c(0.3627, 0.1426, 0.0985, 0.0731, 0.0548), acc = c(94.98, 96.39, 97.22, 97.61, 97.31), time=23.8)

sinu.05.5 = list(loss = c(0.5593, 0.1697, 0.1121, 0.0791, 0.0592), acc = c(93.73, 95.90, 96.86, 97.21, 97.40), time=32.6)
sinu.1.5 = list(loss = c(0.4441, 0.1418, 0.0903, 0.0626, 0.0443), acc = c(94.72, 96.07, 97.33, 97.62, 97.56), time=32.1)
sinu.3.5 = list(loss = c(0.4462, 0.1238, 0.0764, 0.0541, 0.0385), acc = c(95.63, 96.87, 97.40, 97.53, 97.80), time=31.9)
sinu.05.3 = list(loss = c(0.5732, 0.1827, 0.1220, 0.0884, 0.0659), acc = c(93.55, 95.73, 96.59, 97.19, 97.47), time=33.3)
sinu.1.3 = list(loss = c(0.5732, 0.1827, 0.1220, 0.0884, 0.0659), acc = c(93.55, 95.73, 96.59, 97.19, 97.47), time=31)
sinu.3.3 = list(loss = c(0.5715, 0.1797, 0.1203, 0.0871, 0.0654), acc = c(93.58, 95.68, 96.64, 97.21, 97.37), time=28.2)
sinu.05.1 = list(loss = c(0.6488, 0.2056, 0.1429, 0.1078, 0.0840), acc = c(92.46, 94.94, 96.20, 96.65, 97.11), time=28)
sinu.1.1 = list(loss = c(0.5806, 0.1911, 0.1322, 0.0977, 0.0755), acc = c(92.90, 95.35, 96.28, 96.83, 97.22), time=34)
sinu.3.1 = list(loss = c(0.5004, 0.1643, 0.1088, 0.0784, 0.0582), acc = c(94.18, 95.91, 96.75, 97.40, 97.62), time=33.4)


par(mar=c(4,4,0,0))

### LOSS
plot(sigm$loss, ylim=c(0.05,0.75), ylab='Loss', xlab='Epoch', type='o', col='black', lwd=5)
lines(gelu$loss, type='o', col='grey', lwd=5)
lines(swish$loss, type='o', col='yellow', lwd=5)
lines(sinu.05.5$loss, type='o', col='brown', lwd=2)
lines(sinu.05.3$loss, type='o', col='red', lwd=2)
lines(sinu.05.1$loss, type='o', col='orange', lwd=2)
lines(sinu.1.5$loss, type='o', col='purple', lwd=2)
lines(sinu.1.3$loss, type='o', col='blue', lwd=2)
lines(sinu.1.1$loss, type='o', col='skyblue', lwd=2)
lines(sinu.3.5$loss, type='o', col='darkgreen', lwd=2)
lines(sinu.3.3$loss, type='o', col='green', lwd=2)
lines(sinu.3.1$loss, type='o', col='lightgreen', lwd=2)
legend('top', legend=c('Sigmoid', 'GELU', 'Swish'), fill=c('black', 'grey', 'yellow'))
dists = expand.grid(c(5,3,1), c(0.5, 1, 3)) |> apply(1, function(x) paste0("Sinu(", x[2], ",", x[1], ")"))
legend('topright', legend=dists, fill=c('brown','red','orange','skyblue','blue','purple','darkgreen','green','lightgreen'))

dev.copy2pdf(file='4.9-NN-Loss.pdf')

### ACCURACY
plot(sigm$acc, ylim=c(90,100), ylab='Accuracy %', xlab='Epoch', type='o', col='black', lwd=5)
lines(gelu$acc, type='o', col='grey', lwd=5)
lines(swish$acc, type='o', col='yellow', lwd=5)
lines(sinu.05.5$acc, type='o', col='brown', lwd=2)
lines(sinu.05.3$acc, type='o', col='red', lwd=2)
lines(sinu.05.1$acc, type='o', col='orange', lwd=2)
lines(sinu.1.5$acc, type='o', col='purple', lwd=2)
lines(sinu.1.3$acc, type='o', col='blue', lwd=2)
lines(sinu.1.1$acc, type='o', col='skyblue', lwd=2)
lines(sinu.3.5$acc, type='o', col='darkgreen', lwd=2)
lines(sinu.3.3$acc, type='o', col='green', lwd=2)
lines(sinu.3.1$acc, type='o', col='lightgreen', lwd=2)
legend('top', legend=c('Sigmoid', 'GELU', 'Swish'), fill=c('black', 'grey', 'yellow'))
dists = expand.grid(c(5,3,1), c(0.5, 1, 3)) |> apply(1, function(x) paste0("Sinu(", x[2], ",", x[1], ")"))
legend('bottomright', legend=dists, fill=c('brown','red','orange','skyblue','blue','purple','darkgreen','green','lightgreen'))

dev.copy2pdf(file='4.9-NN-Acc.pdf')

barplot(c(28.4, 25.4, 23.8), xlab=c())


dev.off()

