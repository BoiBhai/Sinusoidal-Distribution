potential = function(g, pot=hel_pot){
  return(function(x,y) pot(g(x), y))
}

hel_pot = function(g,y) (sqrt(y) - sqrt(g))^2
dpd_pot = function(g,y, a) (y^(1+a) - (1+1/a)*g*y^a + 1/a*g^(1+a))

sinu_potential = potential(g = function(x) dsinu(x,0,1,1,1))

sinu_potential(3,2)

a=0.1
sinu_a_pot = potential(g = function(x) dsinu(x,0,1,1,1), pot = function(g,y) dpd_pot(g,y,a))

# 1. Prepare Data
x_seq <- seq(-0.5, 1.5, length.out = 200)
y_seq <- seq(0, 10, length.out = 500)
z_matrix <- outer(x_seq, y_seq, Vectorize(sinu_a_pot))

# 2. Setup Plotting Device (Matrix 1: Main Plot, Matrix 2: Legend)
layout(matrix(1:2, ncol = 2), widths = c(4, 1))
cols <- hcl.colors(100, "viridis")

# 3. Main Image Plot
par(mar = c(5, 4, 4, 1)) # Adjust margins
image(x_seq, y_seq, z_matrix, 
      col = cols,
      xlab = "Abscissa (x)", 
      ylab = "Candidate Value (y)",
      main = "Divergence Potential Map")

# Add the 'stuff' (support boundaries and target curve)
abline(v = c(0, 1), col = "white", lty = 2)
x_line <- seq(0, 1, length.out = 100)
lines(x_line, dsinu(x_line, 0, 1, 1, 1), col = "cyan", lwd = 2)

# 4. Colorbar Legend
par(mar = c(5, 0.5, 4, 3)) # Tighten margins for the strip
z_range <- range(z_matrix, na.rm = TRUE)
legend_image <- as.matrix(seq(z_range[1], z_range[2], length.out = 100))

image(1, seq(z_range[1], z_range[2], length.out = 100), 
      t(legend_image), 
      col = cols, 
      axes = FALSE, 
      xlab = "", ylab = "")
axis(4, at = round(seq(z_range[1], z_range[2], length.out = 5), 2), las = 2)
mtext("Potential", side = 4, line = 2, cex = 0.8)

# Reset layout for future plots
layout(1)