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

mydensity = function(x) 0.7*dnorm(x) + 0.3*dnorm(x,6)
avec = 1
pdf(file=paste0('divpot contamin ', range(avec), '.pdf'), width = 8, height = 6)


### animation slice
#ani.options(interval = 0.2, ani.width = 800, ani.height = 600)
#saveGIF({
###

for (a in avec){
mypot = function(g,y) dpd_pot(g,y, a)
pot_xy = potential_g(mydensity, mypot)

x_seq <- seq(-5, 11, length.out = 300)
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

dev.off()  # pdf device

# Reset layout for future plots
layout(1)

### animation slice
#}, movie.name = "dpd_alpha_evolution.gif")
###