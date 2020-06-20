context("Obtaining uncertainty intervals from BCEA::bcea() objects")

test_that("fieller method with indices", {
  # The ex_simple dataset is constructed such that incremental costs and
  # effects are bivariate normally distributed with mean 5000 and 0.2, and
  # have standard deviations 1000 * sqrt(3) and 0.2 and are negatively
  # correlated (\eqn{rho = -0.2 * sqrt(3) \approx 0.3464}).
  z2 <- qnorm(0.975) ^ 2
  b <- 5000 * 0.2 - z2 * (-0.2 * sqrt(3)) * (1000 * sqrt(3)) * 0.2
  a <- 0.2 ^ 2 - z2 * 0.2 ^ 2
  c <- 5000 ^ 2 - z2 * 3000000
  expected_lcl <- (b - sqrt(b^2 - a * c)) / a
  expected_ucl <- (b + sqrt(b^2 - a * c)) / a

  ci <- confint(ex_simple, method = "fieller", comparison = 1)

  expect_equivalent(expected_lcl, ci[1], tolerance = 1e-3)
  expect_equivalent(expected_ucl, ci[2], tolerance = 1e-3)
})
