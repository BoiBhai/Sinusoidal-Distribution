xbar.vec=c()
n = 1000
for (i in 1:n) {
  xbar.vec[i] = mean(rsinustd(1000, 1,1))
  print(i)
}

hist(xbar.vec)
### extremely time consuming