library(plotly)

# Normalization constant helper
get_C_inv <- function(s, k) {
  integrate(function(t) sin(pi * t^s)^k, 0, 1)$value
}

# 2D Sinusoidal Density function
dsinu_2d <- function(z1, z2, s1, k1, s2, k2) {
  c1 <- get_C_inv(s1, k1)
  c2 <- get_C_inv(s2, k2)
  (1/c1 * sin(pi * z1^s1)^k1) * (1/c2 * sin(pi * z2^s2)^k2)
}

# Grid setup
grid_size <- 100
z1_seq <- seq(0, 1, length.out = grid_size)
z2_seq <- seq(0, 1, length.out = grid_size)

# Parameters (Example: asymmetric warping)
s1 <- 0.7; k1 <- 3
s2 <- 2.0; k2 <- 2

# Compute the density matrix
dens_matrix <- outer(z1_seq, z2_seq, function(x, y) dsinu_2d(x, y, s1, k1, s2, k2))

# Create Interactive Plot
plot_ly(x = ~z1_seq, y = ~z2_seq, z = ~dens_matrix) %>% 
  add_surface(
    colorscale = "Viridis",
    colorbar = list(title = "Density")
  ) %>%
  layout(
    title = list(text = "Interactive Bivariate Sinusoidal Density"),
    scene = list(
      xaxis = list(title = "z1"),
      yaxis = list(title = "z2"),
      zaxis = list(title = "f(z1, z2)")
    )
  )