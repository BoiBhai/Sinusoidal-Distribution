source('1-sinudist.R')

sinu.kernel = function(data, h = 1, s = 1, k = 1) {
  n = length(data)
  a_vals = data - h
  d_val = 2 * h
  
  kernel = function(x) {
    sum(vapply(a_vals, function(a) dsinu(x, a, d_val, s, k), numeric(1))) / (n)
  }
  
  return(Vectorize(kernel))
}


data1 = rnorm(1000)
plot(density(data1))
kde1 = sinu.kernel(data1, h=0.2, s=1, k=1)
curve(kde1, add=T, col='red')

###

integrand1 = function(u,s,k) u^2 * dsinu(u,-1,2,s,k)
integrand2 = function(u,s,k) dsinu(u,-1,2,s,k)^2

integral1 = function(s,k) integrate(function(u) integrand1(u,s,k), lower=0, upper=1)$value
integral2 = function(s,k) integrate(function(u) integrand2(u,s,k), lower=0, upper=1)$value

sinu.eff = function(s,k) sqrt(integral1(s,k)) * integral2(s,k)

sinu.eff(3,2)  # HOW IS IT GREATER THAN 1 for a=0, d=1??? HOW IS IT ALSO GREATER FOR LARGE ENOUGH s,k???

##########################################################


x_seq <- seq(0, 10, length.out = 300)
y_seq <- seq(0, 10, length.out = 300)
z_matrix <- outer(x_seq, y_seq, Vectorize(sinu.eff))
z_range <- range(z_matrix, na.rm = TRUE)
#z_range = c(0,1)
heatwalls = c(min(x_seq)+2, max(x_seq)-2)

layout(matrix(1:2, ncol = 2), widths = c(4, 0.5))
cols <- hcl.colors(100, "Temps")

# 3. Main Image Plot
par(mar = c(2, 4, 1, 1)) # Adjust margins
image(x_seq, y_seq, z_matrix, zlim=z_range,
      col = cols,
      xlab = , 
      ylab = "Candidate Density (y)")


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

#dev.off()  # pdf device

# Reset layout for future plots
layout(1)

