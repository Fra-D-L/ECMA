z.test <- function(x, sigma, mu = 0) {
  n <- length(x)
  xbar <- mean(x)
  Z <- (xbar - mu) / (sigma / sqrt(n))
  p_val <- 2 * (1 - pnorm(abs(Z)))
  if (n < 30) {
    cat("\tWarning!\nA sample size of ", n, " is too small to perform an accurate Z test (n should be ≥ 30)\n\n")
  }
  cat("\tZ-test\nZ = ", Z, "\tp-value =", p_val, "\n")
}

z.test(
  c(105, 105, 105, 107, 103, 110, 100, 205, 5, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105,
    105, 105, 105, 105, 105, 115, 95, 105),
  sigma=15,
  mu=100
)
