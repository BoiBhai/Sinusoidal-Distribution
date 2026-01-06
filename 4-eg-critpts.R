omega.vec = c(0.2, 0.4, 0.6, 0.8, 1:6)
chi.vec = c(0.2, 0.4, 0.6, 0.8, 1:6)
xi.vec = c()

d=99; p=d/100
for (chi in chi.vec) {
  for (omega in omega.vec) {
    xi.vec = c(xi.vec, round(qsinu(p, omega=omega, chi=chi), 2))
  }
}

mat1 = matrix(xi.vec, ncol=10, byrow=T)
rownames(mat1) = chi.vec
colnames(mat1) = omega.vec

filename=paste0('critPts',d,'.csv')
write.table(mat1, file=filename, quote=F, sep=',')