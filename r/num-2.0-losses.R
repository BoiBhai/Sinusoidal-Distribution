L2_loss(f=function(x) dnorm(x, -10), g=function(x) dnorm(x))
Hel_gain(f=function(x) dnorm(x, -10), g=function(x) dnorm(x))
Hel_dist(f=function(x) dnorm(x, -10), g=function(x) dnorm(x))
KL_loss(f=function(x) dnorm(x, -10), g=function(x) dnorm(x)) #unbounded error
KL_dist(f=function(x) dnorm(x, -10), g=function(x) dnorm(x)) # unbounded error
symmKL_loss(f=function(x) dnorm(x, -10), g=function(x) dnorm(x)) # unbounded
JS_loss(f=function(x) dnorm(x, -10), g=function(x) dnorm(x)) # unbounded
DPD_dist(f=function(x) dnorm(x, -10), g=function(x) dnorm(x), 0.2)