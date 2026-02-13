# --------------------------------------------------------------------
# arfima_estimates.R
# Estimate memory parameter μ for all sites and save arfima_estimates.csv
# --------------------------------------------------------------------

# Required packages
library(fracdiff)
library(dplyr)
library(readr)

# --------------------------------------------------------------------
# 1. Load site metadata
# --------------------------------------------------------------------
# Assumes: data/site_metadata.csv exists (as in your synthetic example)
meta <- read_csv("data/site_metadata.csv")

# Optional: check structure
# glimpse(meta)

# --------------------------------------------------------------------
# 2. Helper: load a site's time series and extract state variable
# --------------------------------------------------------------------
load_site_timeseries <- function(site_id, ecosystem) {
  # Map ecosystem to subfolder and state variable column name
  if (ecosystem == "coral_reef") {
    path <- file.path("data", "coral_reefs", paste0(tolower(site_id), ".csv"))
    state_col <- "Coral_Cover_Percent"
  } else if (ecosystem == "savannah_forest") {
    path <- file.path("data", "savannah_forest", paste0(tolower(site_id), ".csv"))
    state_col <- "Woody_Cover_Percent"
  } else if (ecosystem == "lake") {
    path <- file.path("data", "lakes", paste0(tolower(site_id), ".csv"))
    state_col <- "Chlorophyll_a_ugL"
  } else if (ecosystem == "lter_stable") {
    path <- file.path("data", "lter_stable", paste0(tolower(site_id), ".csv"))
    state_col <- "Biomass_Index"
  } else {
    stop("Unknown ecosystem: ", ecosystem)
  }

  if (!file.exists(path)) {
    stop("Time series file not found for site: ", site_id, " at ", path)
  }

  dat <- read_csv(path, show_col_types = FALSE)

  if (!state_col %in% names(dat)) {
    stop("State variable column '", state_col, "' not found in file for site: ", site_id)
  }

  # Return numeric vector of state variable
  as.numeric(dat[[state_col]])
}

# --------------------------------------------------------------------
# 3. Helper: fit ARFIMA( p,d,q ) and extract μ = 1 - d
# --------------------------------------------------------------------
estimate_arfima_mu <- function(x, site_id) {
  x <- na.omit(x)

  # Guard against very short series
  if (length(x) < 30) {
    return(list(
      mu = NA_real_,
      se_mu = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      converged = FALSE
    ))
  }

  # Center and scale to help convergence
  x_std <- scale(x)[, 1]

  # Try a small grid of models and pick the best by BIC
  candidates <- expand.grid(p = 0:2, q = 0:2)
  best_bic <- Inf
  best_fit <- NULL

  for (k in seq_len(nrow(candidates))) {
    p <- candidates$p[k]
    q <- candidates$q[k]

    # fracdiff() can fail; use try()
    fit <- try(
      fracdiff(x_std, nar = p, nma = q),
      silent = TRUE
    )

    if (inherits(fit, "try-error"))
      next

    # fracdiff object has log.likelihood, npar, n
    k_param <- length(fit$coef) + 1  # +1 for d
    bic <- -2 * fit$log.likelihood + k_param * log(fit$n)

    if (bic < best_bic) {
      best_bic <- bic
      best_fit <- fit
    }
  }

  if (is.null(best_fit)) {
    warning("ARFIMA did not converge for site: ", site_id)
    return(list(
      mu = NA_real_,
      se_mu = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      converged = FALSE
    ))
  }

  # Memory parameter μ = 1 - d
  d_hat <- best_fit$d
  mu_hat <- 1 - d_hat

  # Standard error of d from covariance matrix
  if (!is.null(best_fit$covariance)) {
    se_d <- sqrt(best_fit$covariance[1, 1])
  } else {
    se_d <- NA_real_
  }

  se_mu <- se_d
  ci_lower <- mu_hat - 1.96 * se_mu
  ci_upper <- mu_hat + 1.96 * se_mu

  list(
    mu = as.numeric(mu_hat),
    se_mu = as.numeric(se_mu),
    ci_lower = as.numeric(ci_lower),
    ci_upper = as.numeric(ci_upper),
    converged = TRUE
  )
}

# --------------------------------------------------------------------
# 4. Loop over all 84 sites and estimate μ
# --------------------------------------------------------------------
results <- vector("list", nrow(meta))

for (i in seq_len(nrow(meta))) {
  site_id  <- meta$Site_ID[i]
  eco      <- meta$Ecosystem[i]

  message("Processing site ", i, "/", nrow(meta), ": ", site_id, " (", eco, ")")

  ts_vec <- load_site_timeseries(site_id, eco)

  est <- estimate_arfima_mu(ts_vec, site_id)

  results[[i]] <- tibble(
    Site_ID   = site_id,
    Ecosystem = eco,
    mu        = est$mu,
    SE_mu     = est$se_mu,
    CI_lower  = est$ci_lower,
    CI_upper  = est$ci_upper,
    Method    = "ARFIMA",
    Converged = est$converged
  )
}

arfima_estimates <- bind_rows(results)

# --------------------------------------------------------------------
# 5. Save to CSV
# --------------------------------------------------------------------
if (!dir.exists("results")) dir.create("results")

write_csv(arfima_estimates, "results/arfima_estimates.csv")

message("✅ Saved results/arfima_estimates.csv with ", nrow(arfima_estimates), " rows.")
