dwilcoxon = Vectorize(function(x, m, n) {
  N <- m + n
  
  # Possible range check
  min_w <- sum(1:m)
  max_w <- sum((N - m + 1):N)
  
  if (x < min_w || x > max_w) return(0)
  
  # DP table:
  # dp[k+1, s+1] = number of ways to choose k ranks summing to s
  dp <- matrix(0, nrow = m + 1, ncol = max_w + 1)
  dp[1, 1] <- 1  # 0 elements, sum 0
  
  for (r in 1:N) {
    for (k in min(r, m):1) {
      for (s in max_w:r) {
        dp[k + 1, s + 1] <- dp[k + 1, s + 1] + dp[k, s - r + 1]
      }
    }
  }
  
  count <- dp[m + 1, x + 1]
  total <- choose(N, m)
  
  return(count / total)
}, vectorize.args='x')

dwilcoxon(0:18, 3,4)*35

dwilcox(0:18, 3,2)*10  # actually Mann Whitney U
pwilcox(0:18, 3,2)*10


fit.Fsinu.L2