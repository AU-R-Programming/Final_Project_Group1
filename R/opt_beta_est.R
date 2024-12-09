
#Task 1:


# Calculate initial beta values, least squares = (XtX)^-1 %*% XTy
initial_beta <- function(X, y) {
  solve(t(X)%*%X) %*% t(X)%*%y
}


# loss function function
# loss_func <- function(beta, X, y) {
#   p <- 1 / (1 + exp(-X %*% beta))
#   sum(-y * log(p) - (1 - y) * log(1 - p))
# }



# test loss
loss_func <- function(beta, X, y) {
  p <- 1 / (1 + exp(-X %*% beta))
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)  # Clip probabilities
  sum(-y * log(p) - (1 - y) * log(1 - p))
}

# beta optimization function
estimate_beta <- function(X, y, beta_initial) {
  optim(
    par = beta_initial,
    fn = loss_func,
    X = X,
    y = y,
    method = "BFGS"  # optimization method
  )
}

# putting above functions together

#' Beta optimization function
#'
#' This function calculates the optimal values for the parameter beta when provided with a matrix of data called X, and
#' the the corresponding y binary response vector. When these values are input, the function will find an initial beta
#' beta estimate, then uses an optimization function to refine the beta estimates until they are sufficiently near
#' the true values of beta.
#'
#' @param X This is a matrix of data with dimensions n x p.
#' @param y This is a vector with a length of n.
#' @return This function will return an estimate of the parameter beta, along with an intercept.
#' @export
#' @examples
#' n <- 100
#' p <- 3
#' p_i <- 1 / (1 + exp(-design %*% beta_true))
#' y <- rbinom(n, size = 1, prob = p_i)
#' beta_opt <- opt_beta_est(X = X, y = y)
#' print(beta_opt)

opt_beta_est <- function(X, y){
  #n <- nrow(X)  # calculate the number of rows in X
  #X <- cbind(rep(1, n), X) # adding intercept
  beta_initial <- initial_beta(X = X, y = y)
  beta_opt <- estimate_beta(X = X, y = y, beta_initial = as.vector(beta_initial))
  #cat("Estimated beta:", beta_opt$par, "\n")
  return(beta_opt$par)
}


# testing code
set.seed(42)  # for reproducibility
n <- 100   # number of observations
p <- 3     # number of predictors
beta_true <- c(1.25, 0.5, -1, 0.75)  # true coefficients

X <- matrix(rnorm(n * p), nrow = n, ncol = p)   # predictor matrix
design <- cbind(1, X)                           # add intercept column
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



