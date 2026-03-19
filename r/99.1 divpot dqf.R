library(animation)

################################################################################
# PRELIMINARY DEFINITIONS

potential_g = function(g, pot_fn){
  return(function(x,y) pot_fn(g(x), y))
}

hel_pot = function(g,y) (sqrt(y) - sqrt(g))^2
dpd_pot = function(g,y, a) (y^(1+a) - (1+1/a)*g*y^a + 1/a*g^(1+a))
kl_pot = function(g,y) log(y/g)*y

################################################################################
# OUR REQUIREMENTS

mypdf = function(x) 0.7*dnorm(x) + 0.3*dnorm(x, 6)
mycdf = function(x) 0.7*pnorm(x) + 0.3*pnorm(x, 6)

myqf = function(p) {
  sapply(p, function(prob) {
    # Handle boundary cases for curve() and numeric stability
    if (prob <= 0) return(-Inf)
    if (prob >= 1) return(Inf)
    
    # Expand interval to [-20, 30] to safely cover the N(6, 1) tail
    uniroot(function(x) mycdf(x) - prob, interval = c(-20, 30), extendInt = "yes")$root
  })
}

mydqf = function(p) {
  quantiles = myqf(p)
  # Density of infinite quantiles is 0
  res = mypdf(quantiles)
  res[is.na(res)] = 0
  return(res)
}

# Use n to increase resolution for the bimodal density-quantile shape
curve(mydqf, from = 0, to = 1, n = 500, xlab = "p", ylab = "f(Q(p))", main = "Density Quantile Function")

#pdf(file='divpot_contamin.pdf', width = 8, height = 6)

mydensity = mydqf

### animation slice
ani.options(interval = 0.2, ani.width = 800, ani.height = 600)

saveGIF({
  ###
  
  for (a in c(1:10/100, 1:20/20)){
    mypot = function(g,y) dpd_pot(g,y, a)
    pot_xy = potential_g(mydensity, mypot)
    
    x_seq <- seq(-2, 3, length.out = 300)
    y_seq <- seq(0, 2*max(mydensity(x_seq)), length.out = 300)
    z_matrix <- outer(x_seq, y_seq, Vectorize(pot_xy))
    #z_range <- range(z_matrix, na.rm = TRUE)
    z_range = c(0,1)
    heatwalls = c(min(x_seq)+2, max(x_seq)-2)
    
    ################################################################################
    # PLOTTING
    
    
    
    # 2. Setup Plotting Device (Matrix 1: Main Plot, Matrix 2: Legend)
    layout(matrix(1:2, ncol = 2), widths = c(4, 0.5))
    cols <- hcl.colors(100, "Temps")
    
    # 3. Main Image Plot
    par(mar = c(2, 4, 1, 1)) # Adjust margins
    image(x_seq, y_seq, z_matrix, zlim=z_range,
          col = cols,
          xlab = a, 
          ylab = "Candidate Density (y)")
    
    # Add the 'stuff' (support boundaries and target curve)
    abline(v = heatwalls, col = "white", lty = 2)
    lines(x_seq, mydensity(x_seq), col = "green", lwd = 2)
    
    # 4. Colorbar Legend
    par(mar = c(0.5, 0, 1, 2)) # Tighten margins for the strip
    legend_image <- as.matrix(seq(z_range[1], z_range[2], length.out = 100))
    
    image(1, seq(z_range[1], z_range[2], length.out = 100), 
          t(legend_image), zlim=z_range,
          col = cols, 
          axes = FALSE, 
          xlab = "", ylab = "")
    axis(4, at = round(seq(z_range[1], z_range[2], length.out = 5), 2), las = 2)
    mtext("Potential", side = 4, line = 3, cex = 0.8)
  }
  
  #dev.off()  # pdf device
  
  # Reset layout for future plots
  layout(1)
  
  ### animation slice
}, movie.name = "dpd_alpha_evolution.gif")
###