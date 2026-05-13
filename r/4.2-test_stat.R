# =========================================================
# Sinusoidal t-test approximation under H0 : mu = 0
# =========================================================
dev.new(width = 8, height = 6, noRStudioGD = TRUE)
par(mar = c(4, 4, 0, 0))

n = 50
B = 100

# =========================================================
# Placeholder sinusoidal parameters
# =========================================================

d.true = 2
s.true = 3
k.true = 4

a.true = -sinu.mean(0,d.true,s.true,k.true)

# =========================================================
# Generate observed sample under H0
# =========================================================

x.obs = rsinu(
  n,
  a.true,
  d.true,
  s.true,
  k.true
)

# =========================================================
# Observed t-statistic
# =========================================================

t.obs =
  sqrt(n) * mean(x.obs) / sd(x.obs)

t.obs


# =========================================================
# Monte Carlo null distribution
# =========================================================

t.samples = numeric(B)

for(b in 1:B)
{
  
  # -----------------------------------------
  # Generate sinusoidal sample under H0
  # -----------------------------------------
  
  x = rsinu(
    n,
    a.true,
    d.true,
    s.true,
    k.true
  )
  
  
  # -----------------------------------------
  # Compute t-statistic
  # -----------------------------------------
  
  t.samples[b] =
    sqrt(n) * mean(x) / sd(x)
}


# =========================================================
# Fit sinusoidal distribution to t-statistics
# =========================================================

fit.t = fitsinu.ecdf(
  t.samples
)

fit.t


# =========================================================
# Extract fitted parameters
# =========================================================

a.t = fit.t$par[1]
d.t = fit.t$par[2]
s.t = fit.t$par[3]
k.t = fit.t$par[4]


# =========================================================
# Compute sinusoidal p-value
# =========================================================

left.p =
  psinu(
    t.obs,
    a.t,
    d.t,
    s.t,
    k.t
  )

right.p =
  1 - left.p

p.value =
  2 * min(left.p, right.p)

p.value


# =========================================================
# Classical t-test comparison
# =========================================================

t.test(x.obs, mu=0)


# =========================================================
# Diagnostics
# =========================================================

plot(
  ecdf(t.samples),
  main = '',
  xlab = "t",
  ylab = "F(t)",
  lwd = 2,
  col='green'
)

curve(
  psinu(
    x,
    a.t,
    d.t,
    s.t,
    k.t
  ),
  add=TRUE,
  lwd=2,
  col='red'
)

abline(v=t.obs, lwd=2, lty=2)

dev.copy2pdf(file='4-teststat.pdf')
#dev.off()