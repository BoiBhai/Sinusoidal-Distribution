sineArea = function(alpha=0, delta=1, omega=1, chi=1, switch=F) {
  sineCurve = function(x) {
      if (switch==F) {
        (sin( pi * ((x-alpha)/delta)^omega ))^chi
      } else {
        (sin( pi * ((alpha+delta - x)/delta)^omega ))^chi
      }
  }

  areaOut = integrate(sineCurve, lower=alpha, upper=alpha+delta)$value
  return(areaOut)
}

sineArea(0,1,1,1)

sineArea.delta = Vectorize(function(delta) {
  sineArea(0, delta, 1, 1)
}, vectorize.args='delta')

sineArea.omega = Vectorize(function(omega) {
  sineArea(0, 1, omega, 1)
}, vectorize.args='omega')

sineArea.chi = Vectorize(function(chi) {
  sineArea(0,1,1, chi)
}, vectorize.args='chi')

curve(sineArea.delta(x), xlim=c(1,10))
curve(sineArea.omega(x), xlim=c(0,10))
curve(sineArea.chi(x), xlim=c(0,10))