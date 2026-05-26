# check_z
# Debug: Check current dimensions of Zst2
cat("Zst2 dimensions:", dim(Zst2), "\n")
cat("Expected dimensions based on data:\n")
cat("  Parks (npk):", npk, "\n")
cat("  Max sites:", max(nsite_pk), "\n") 
cat("  Years:", length(years), "\n")

# Check what the model expects
cat("Unique values in y data:\n")
cat("  Parks in y:", sort(unique(y[,2])), "\n")
cat("  Sites in y:", range(y[,3]), "\n")
cat("  Years in y:", sort(unique(y[,4])), "\n")

# Fix: Rebuild Zst2 with correct dimensions
npk_actual <- length(unique(y[,2]))
max_sites_actual <- max(y[,3])
nyears_actual <- length(unique(y[,4]))

cat("Rebuilding Zst2 with correct dimensions...\n")

Zst2_fixed <- array(NA, 
                   dim = c(npk_actual, max_sites_actual, nyears_actual),
                   dimnames = list(
                     paste0("park_", 1:npk_actual),
                     paste0("site_", 1:max_sites_actual), 
                     paste0("year_", 1:nyears_actual)
                   ))

# Fill the array with initial values
for(a in 1:nrow(Zst)){
  zl <- Zst[a,]
  r <- as.numeric(zl$parkey)
  j <- as.numeric(zl$site_n) 
  t <- as.numeric(zl$year_n)
  
  # Check bounds before assignment
  if(r <= npk_actual && j <= max_sites_actual && t <= nyears_actual) {
    Zst2_fixed[r, j, t] <- as.numeric(zl$z)
  }
}

# Update the inits function
inits <- function() {
    list(
        Z = Zst2_fixed,  # Use the corrected array
        beta = rnorm(n_bs, 0.5),
        mu.alpha0 = rnorm(1, 0.5),
        alpha = rnorm(n_as, 0.5)
    )
}