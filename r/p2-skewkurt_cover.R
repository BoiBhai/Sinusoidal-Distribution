# 1. Define the parameter space
vals <- seq(0.05, 6, by=0.05)
sk.grid <- expand.grid(s = vals, k = vals)

# Initialize columns with -99 (or NA) to track progress
sk.grid$skew <- -99
sk.grid$kurt <- -99

# 2. Define the robust wrapper (as before)
safe_calc <- function(fun, s_val, k_val) {
  tryCatch({
    res <- fun(s_val, k_val)
    if (is.na(res) || is.nan(res) || is.infinite(res)) -99 else res
  }, error = function(e) -99)
}

# 3. Iterative Loop with Periodic Saving
save_interval <- 50  # Save every 50 iterations

for (i in 1:nrow(sk.grid)) {
  # Calculate moments for the current row
  sk.grid$skew[i] <- safe_calc(sinu.skew, sk.grid$s[i], sk.grid$k[i])
  sk.grid$kurt[i] <- safe_calc(sinu.kurt, sk.grid$s[i], sk.grid$k[i])
  
  # Periodic checkpointing
  if (i %% save_interval == 0) {
    saveRDS(sk.grid, "sinu_moments_data_checkpoint.rds")
    message(sprintf("Iteration %d of %d saved.", i, nrow(sk.grid)))
  }
}

# 4. Final Save
saveRDS(sk.grid, "sinu_moments_data_final.rds")





########################################################





dev.new(width = 8, height = 6, noRStudioGD = TRUE)

# 1. Setup Layout
layout(matrix(1:2, ncol = 2), widths = c(4, 1.5))
par(mar = c(4, 4, 0, 0))

# 2. Main Plot (using your existing data)
s_norm <- (sk_valid$s - min(sk.grid$s)) / (max(sk.grid$s) - min(sk.grid$s))
k_norm <- (sk_valid$k - min(sk.grid$k)) / (max(sk.grid$k) - min(sk.grid$k))
pt_colors <- rgb(s_norm, k_norm, 0)

plot(sk_valid$skew, sk_valid$kurt, 
     ylim = c(-1, 4), xlim = c(-1, 2), 
     col = pt_colors, pch = 19, cex = 0.8,
     xlab='Skewness', ylab='Kurtosis')
grid(lty = "dotted", col = "gray")

# 3. 2D Legend with Dense Ticks
par(mar = c(3, 0.5, 0, 3)) # Extra margin for the right-side labels
leg_res <- 100
leg_vals <- seq(0, 1, length.out = leg_res)
leg_grid <- expand.grid(s = leg_vals, k = leg_vals)
leg_colors <- rgb(leg_grid$s, leg_grid$k, 0)

# Generate the color gradient square
plot(leg_grid$s, leg_grid$k, 
     col = leg_colors, pch = 15, cex = 1.2,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")

# --- TICK CONFIGURATION ---
# Define the density of ticks (e.g., every 0.1 units in parameter space)
tick_steps <- seq(0, 1, by = 0.1)

# Calculate the actual values for the labels based on your 'vals' range
s_range <- range(sk.grid$s)
k_range <- range(sk.grid$k)
s_labs <- round(seq(s_range[1], s_range[2], length.out = length(tick_steps)), 2)
k_labs <- round(seq(k_range[1], k_range[2], length.out = length(tick_steps)), 2)

# X-axis (s parameter)
axis(1, at = tick_steps, labels = s_labs, cex.axis = 0.7, las = 2)
mtext("s", side = 1, line = 2, cex = 0.9)

# Y-axis (k parameter - placed on the right)
axis(4, at = tick_steps, labels = k_labs, cex.axis = 0.7, las = 1)
mtext("k", side = 4, line = 2, cex = 0.9)

# Reset
layout(1)

dev.copy2pdf(file='2-skewkurtcover.pdf')
dev.off()