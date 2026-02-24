qsinu.omegaChi = function(p, omega, chi, switch=F) {
  qsinu(p, 0,1, omega, chi, switch)
}

omega.vec = c(1:4*2/10, 1:8)
chi.vec = c(1:9/10, 1:15)
m = length(omega.vec)
n = length(chi.vec)
p = 0.95

q.mat = matrix(0, ncol=m, nrow=n)

for (i in 1:n) {
  for (j in 1:m) {
      q.mat[i,j] = round(qsinu.omegaChi(p, omega.vec[j], chi.vec[i]),3)
  }
}

rownames(q.mat)=chi.vec; colnames(q.mat)=omega.vec
tab1 = as.table(q.mat)

filename = paste0('critPt',p*100,'.csv')
write.csv(tab1, filename)

#matplot(q.mat, type='o')