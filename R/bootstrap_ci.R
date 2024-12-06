# Task 2: Bootstrapping Confidence Intervals

#' Bootstrap Confidence Intervals
#'
#' This function uses the bootstrap method of resampling from the same population/dataset to creat confidence intervals at
#' the user-specified significance level, alpha. The default significance level is 0.05, and the default number of times the
#' bootstrap procedure will be repeated is 20.
#'
#' @param X This is a matrix of data with dimensions n x p.
#' @param y This is a binary vector of length n.
#' @param beta_opt_func This is the loss function, it is the function that will be repeated for the bootstrap procedure.
#' @param alpha This is the significance level, with a default of 0.05.
#' @param n_bootstrap This is the number of times this procedure will be repeated, with a default of 20.
#'
#' @return This function will return a confidence interval for each element in the vector beta, which is the output of the
#' beta_opt_func function.
#' @export
#'
#' @examples
#' n <- 100   # number of observations
#' p <- 3     # number of predictors
#' beta_true <- c(1.25, 0.5, -1, 0.75)  # true coefficients
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)   # predictor matrix
#' design <- cbind(1, X)                           # add intercept column
#' p_i <- 1 / (1 + exp(-design %*% beta_true))     # probabilities
#' y <- rbinom(n, size = 1, prob = p_i)            # response variable
#' bootstrap_ci(X, y, beta_opt_func, alpha = 0.05, n_bootstrap = 20)

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
    bootstrap_betas[i, ] <- opt_beta_est(X_boot, y_boot)   # Store the estimated beta
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

