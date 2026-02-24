library(plotrix)
s=3
k=2
r_planet = Vectorize(function(z, r0=1) {
  integral = integrate(function(x) tan(x - asin(sin(x)^k)),
                       lower=0, upper=pi*z^s)$value
  return(r0 * exp(integral))
}, vectorize.args = 'z')

thetas = seq(0,pi, length.out=100)
radii = r_planet((thetas/pi)^(1/s))
r_planet(seq(0, 0.8, length.out=100))

radial.plot(lengths = radii, radial.pos = thetas)

curve(tan(x - asin(sin(x)^k)), xlim=c(0,pi))
x=2.23703575
tan(x - asin(sin(x)^k))
    