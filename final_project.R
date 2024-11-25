# final project 

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
  beta_initial <- initial_beta(X = X, y = y)
  beta_opt <- estimate_beta(X = X, y = y, beta_init = beta_initial)
  cat("Estimated beta:", result$par, "\n")
}

# simulating data for function
simulate_data <- function(n, p, true_beta) {
  set.seed(123)  # For reproducibility
  
  # Generate random predictor matrix X (standard normal distribution)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  
  # Calculate p_i
  p_i <- 1 / (1 + exp(-X %*% true_beta)) # probability for y generation
  
  # Generate binary response variable y
  y <- rbinom(n, size = 1, prob = p_i)
  
  list(X = X, y = y)
}

# testing code
set.seed(42)  # For reproducibility
n <- 100   # Number of observations
p <- 3     # Number of predictors
beta_true <- c(0.5, -1, 0.75)  # True coefficients

data <- simulate_data(n, p, beta_true)
X <- data$X
y <- data$y

# calculate initial beta
beta_init <- initial_beta(X, y)

# estimate beta using optimization
result <- estimate_beta(X, y, beta_init)

# display results compared to true values
cat("True beta:", beta_true, "\n")
cat("Estimated beta:", result$par, "\n")

# testing final function 
opt_beta_est(X = X, y = y)
