# final project

#Task 1:


# Calculate initial beta values, least squares = (XtX)^-1 %*% XTy
initial_beta <- function(X, y) {
  solve(t(X)%*%X) %*% t(X)%*%y
}


# loss function function
loss_func <- function(beta, X, y) {
  p <- 1 / (1 + exp(-X %*% beta))
  sum(-y * log(p) - (1 - y) * log(1 - p))
}

# beta optimization function
estimate_beta <- function(X, y, beta_init) {
  optim(
    par = beta_init,
    fn = loss_func,
    X = X,
    y = y,
    method = "BFGS"  # optimization method
  )
}

# putting above functions together

opt_beta_est <- function(X, y){
  #X <- cbind(rep(1, n), X) # adding intercept
  beta_initial <- initial_beta(X = X, y = y)
  beta_opt <- estimate_beta(X = X, y = y, beta_init = beta_initial)
  #cat("Estimated beta:", beta_opt$par, "\n")
  return(beta_opt$par)
}


# testing code
set.seed(42)  # for reproducibility
n <- 100   # number of observations
p <- 3     # number of predictors
beta_true <- c(1.25, 0.5, -1, 0.75)  # true coefficients

X <- matrix(rnorm(n * p), nrow = n, ncol = p)   # predictor matrix
X <- design <- cbind(1, X)                           # add intercept column
p_i <- 1 / (1 + exp(-design %*% beta_true))     # probabilities
y <- rbinom(n, size = 1, prob = p_i)            # response variable


# calculate initial beta
beta_init <- initial_beta(X, y)

# estimate beta using optimization
result <- estimate_beta(X, y, beta_init)

# testing final function
beta_opt <- opt_beta_est(X = X, y = y)

# display results compared to true values
cat("True beta:", beta_true, "\n")
cat("Estimated beta:", beta_opt, "\n")

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

bootstrap_ci(X, y, beta_opt_func, alpha = 0.05, n_bootstrap = 20)




#Task 3:


# Confusion matrix metrics
confusion_metrics <- function(y_true, y_pred) {
  TP <- sum(y_true == 1 & y_pred == 1)
  TN <- sum(y_true == 0 & y_pred == 0)
  FP <- sum(y_true == 0 & y_pred == 1)
  FN <- sum(y_true == 1 & y_pred == 0)

  prevalence <- mean(y_true)
  accuracy <- (TP + TN) / length(y_true)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  FDR <- FP / (FP + TP)
  DOR <- (TP / FN) / (FP / TN)

  return(list(prevalence = prevalence, accuracy = accuracy, sensitivity = sensitivity,
              specificity = specificity, FDR = FDR, DOR = DOR))
}

# Predict probabilities and calculate metrics
#X_design <- cbind(1, X)  # add intercept column, commented out bc this is created above
p_hat <- 1 / (1 + exp(-X %*% beta_opt))  # use design matrix with intercept
y_pred <- ifelse(p_hat > 0.5, 1, 0)  # classify predictions

# Calculate metrics
metrics <- confusion_metrics(y, y_pred)

# Print confusion matrix metrics
print(metrics)

