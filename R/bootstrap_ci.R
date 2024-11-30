# Task 2: Bootstrapping Confidence Intervals

bootstrap_ci <- function(X, y, beta_opt_func, alpha = 0.05, n_bootstrap = 20) {
  n <- nrow(X)                # Number of observations
  p <- ncol(X)                # Number of predictors
  bootstrap_betas <- matrix(0, nrow = n_bootstrap, ncol = p)  # Matrix to store bootstrap beta estimates

  for (i in 1:n_bootstrap) {
    # Resample data with replacement
    sample_indices <- sample(1:n, size = n, replace = TRUE)  # Bootstrap sample indices
    X_boot <- X[sample_indices, ]                           # Bootstrap predictors
    y_boot <- y[sample_indices]                             # Bootstrap response

    # Estimate beta for the bootstrap sample using the provided beta optimization function
    bootstrap_betas[i, ] <- beta_opt_func(X_boot, y_boot)   # Store the estimated beta
  }

  # Compute confidence intervals
  ci_lower <- apply(bootstrap_betas, 2, function(b) quantile(b, alpha / 2))  # Lower quantile
  ci_upper <- apply(bootstrap_betas, 2, function(b) quantile(b, 1 - alpha / 2))  # Upper quantile

  # Create a data frame for the confidence intervals
  ci <- data.frame(
    Coefficient = 1:p,
    Lower_CI = ci_lower,
    Upper_CI = ci_upper
  )

  return(ci)  # Return the confidence intervals
}
