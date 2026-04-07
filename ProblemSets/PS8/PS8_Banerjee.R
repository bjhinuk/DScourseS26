.libPaths(c("~/R/x86_64-pc-linux-gnu-library/4.3", .libPaths()))

# PS8_Jhinuk.R
# Econ 5253 - Spring 2026

library(nloptr)
library(tinytable)
library(modelsummary)

# ============================================================
# Q4: Generate Data
# ============================================================
set.seed(100)

N <- 100000
K <- 10

X   <- cbind(1, matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1))
eps <- rnorm(N, mean = 0, sd = 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y   <- X %*% beta + eps

# ============================================================
# Q5: Closed-Form OLS
# ============================================================
beta_ols_cf <- solve(t(X) %*% X) %*% t(X) %*% Y
cat("Q5 - Closed-form OLS beta:\n")
print(beta_ols_cf)

# ============================================================
# Q6: Gradient Descent
# ============================================================
learning_rate <- 0.0000003
beta_gd       <- rep(0, K)
max_iter      <- 100000
tol           <- 1e-8

for (i in 1:max_iter) {
  gradient_gd <- -2 * t(X) %*% (Y - X %*% beta_gd)
  beta_new    <- beta_gd - learning_rate * gradient_gd
  if (max(abs(beta_new - beta_gd)) < tol) {
    cat("Gradient descent converged at iteration", i, "\n")
    break
  }
  beta_gd <- beta_new
}

cat("Q6 - Gradient Descent beta:\n")
print(beta_gd)

# ============================================================
# Q7: nloptr - L-BFGS and Nelder-Mead
# ============================================================
ols_obj <- function(beta_vec, Y, X) {
  resid <- as.vector(Y - X %*% beta_vec)
  return(sum(resid^2))
}

ols_grad <- function(beta_vec, Y, X) {
  return(as.vector(-2 * t(X) %*% (Y - X %*% beta_vec)))
}

beta0 <- rep(0, K)

result_lbfgs <- nloptr(
  x0          = beta0,
  eval_f      = ols_obj,
  eval_grad_f = ols_grad,
  opts        = list(algorithm = "NLOPT_LD_LBFGS",
                     xtol_rel  = 1e-8,
                     maxeval   = 10000),
  Y = Y, X = X
)
cat("Q7 - L-BFGS beta:\n")
print(result_lbfgs$solution)

result_nm <- nloptr(
  x0     = beta0,
  eval_f = ols_obj,
  opts   = list(algorithm = "NLOPT_LN_NELDERMEAD",
                xtol_rel  = 1e-8,
                maxeval   = 100000),
  Y = Y, X = X
)
cat("Q7 - Nelder-Mead beta:\n")
print(result_nm$solution)

# ============================================================
# Q8: MLE via L-BFGS
# ============================================================
mle_obj <- function(theta, Y, X) {
  beta_vec <- theta[1:(length(theta) - 1)]
  sig      <- theta[length(theta)]
  n        <- nrow(X)
  ll       <- -n/2 * log(2 * pi * sig^2) -
               (1/(2 * sig^2)) * sum((Y - X %*% beta_vec)^2)
  return(-ll)
}

gradient <- function(theta, Y, X) {
  grad     <- as.vector(rep(0, length(theta)))
  beta_vec <- theta[1:(length(theta) - 1)]
  sig      <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta_vec) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig -
                         crossprod(Y - X %*% beta_vec) / (sig^3)
  return(grad)
}

theta0 <- c(rep(0, K), 1)

result_mle <- nloptr(
  x0          = theta0,
  eval_f      = mle_obj,
  eval_grad_f = gradient,
  opts        = list(algorithm = "NLOPT_LD_LBFGS",
                     xtol_rel  = 1e-8,
                     maxeval   = 10000),
  Y = Y, X = X
)
cat("Q8 - MLE beta:\n")
print(result_mle$solution[1:K])
cat("Q8 - MLE sigma:", result_mle$solution[K + 1], "\n")

# ============================================================
# Q9: lm() and modelsummary export
# ============================================================
options("modelsummary_format_numeric_latex" = "plain")

ols_lm <- lm(Y ~ X - 1)

modelsummary(ols_lm,
             output = "PS8_table_Jhinuk.tex",
             title  = "OLS Regression Results",
             stars  = TRUE)

cat("Done! Table written to PS8_table_Jhinuk.tex\n")
