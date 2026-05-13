library(numDeriv)

f1f2 = Vectorize(function(x,s,k) {
  integrand = function(t) {
    
    val = dsinustd(t,s,k) * dsinustd(x-t,s,k)
    
    ifelse(is.finite(val), val, 0)
    
  }
  tryCatch(
    
    integrate(
      integrand,
      lower = 0,
      upper = 1,
      subdivisions = 2000,
      rel.tol = 1e-8,
      stop.on.error = FALSE
    )$value,
    
    error = function(e) NA
    
  )
  
}, vectorize.args='x')




### Mode fitting
optim.f1f2.mode = function(s,k) optim(par=1, function(x) -f1f2(x,s,k), method='L-BFGS-B', lower=0, upper=2)
optim.f1f2.mode(3,2)
f1f2.mode = function(s,k) optim.f1f2.mode(s,k)$par
f1f2.modaldens = function(s,k) - optim.f1f2.mode(s,k)$value
s.fit = function(s,k) 1/(1-log(f1f2.mode(s,k))/log(2))
k.fit = function(s,k) uniroot(function(t) sinuarea(s.fit(s,k),t) - 1/2*1/f1f2.modaldens(s,k), c(0.1,10))$root

# Grid generation
s.vec = c(seq(0.1,1, by=0.1), seq(1.1,2, by=0.1))
k.vec = s.vec
sk.opt = c()
for (s1 in s.vec){
  for (k1 in k.vec){
    s.opt = s.fit(s1,k1)
    k.opt = k.fit(s1,k1)
    hd = Hel_dist(function(x) dsinu(x,0,2,s.opt,k.opt), function(x) f1f2(x,s1,k1))
    print(c(s1, k1, s.opt, k.opt, hd))
    sk.opt = rbind(sk.opt, c(s1, k1, s.opt, k.opt, hd))
  }
}

hel.rect = pmax(0,sk.opt[,5])
hel.log = -log10(hel.rect)
hel.log[is.infinite(hel.log)] = NA
