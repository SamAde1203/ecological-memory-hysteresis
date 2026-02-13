# ARFIMA Analysis Pipeline
# Estimates memory parameters for all sites

library(fracdiff)
library(dplyr)

# Load data
sites <- read.csv("data/site_metadata.csv")

# Function to estimate ARFIMA
estimate_arfima <- function(site_id) {
  # Load site data
  data <- read.csv(paste0("data/", site_id, ".csv"))
  
  # Fit ARFIMA(1,d,1)
  fit <- fracdiff(data$State_Variable, nar=1, nma=1)
  
  # Extract memory parameter
  mu <- 1 - fit$d
  se_mu <- sqrt(fit$cov[1,1])
  
  return(data.frame(
    Site_ID = site_id,
    mu = mu,
    SE = se_mu,
    CI_lower = mu - 1.96*se_mu,
    CI_upper = mu + 1.96*se_mu
  ))
}

# Estimate for all sites
results <- lapply(sites$Site_ID, estimate_arfima) %>% bind_rows()

# Save results
write.csv(results, "results/arfima_estimates.csv", row.names = FALSE)
