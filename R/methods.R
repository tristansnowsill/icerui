#' Fieller method for confidence interval calculation
#'
#' Calculate a confidence interval using the Fieller method by manually
#' providing incremental costs and effects. Consider using \link{icerui}
#' for greater functionality.
#'
#' The Fieller method is a parametric uncertainty interval which is accurate
#' assuming incremental costs and effects follow a bivariate normal
#' distribution.
#'
#' @param delta.e Vector of incremental effects.
#' @param delta.c Vector of incremental costs.
#' @param level   Desired confidence level.
#' @return        A vector with the lower and upper confidence limits.
#'
#' @export
fieller <- function(delta.e, delta.c, level = 0.95) {
  mu_e <- mean(delta.e)
  mu_c <- mean(delta.c)
  vce  <- stats::var(cbind(delta.e, delta.c))
  z2   <- stats::qnorm(1 - (1 - level) / 2) ^ 2
  pt_a <- mu_c ^ 2 - z2 * vce[2, 2]
  pt_b <- mu_e ^ 2 - z2 * vce[1, 1]
  pt_c <- mu_c * mu_e - z2 * vce[1, 2]
  lims <- (pt_c + c(-1, 1) * sqrt(pt_c ^ 2 - pt_a * pt_b)) / pt_b
  lims
}

#' Bootstrap percentile method for confidence interval calculation
#'
#' Calculate a confidence interval using the bootstrap percentile method by
#' manually providing incremental costs and effects. Consider using
#' \link{icerui} for greater functionality.
#'
#' @param delta.e Vector of incremental effects.
#' @param delta.c Vector of incremental costs.
#' @param level   Desired confidence level.
#' @return        A vector with the lower and upper confidence limits.
#'
#' @export
bspercent <- function(delta.e, delta.c, level) {
  mu_e         <- mean(delta.e)
  mu_c         <- mean(delta.c)
  angle_origin <- atan2(-mu_c, -mu_e)
  rel_angles   <- (atan2(delta.c, delta.e) - angle_origin) %% (2 * pi)
  ord          <- order(rel_angles)
  icers        <- (delta.c / delta.e)[ord]
  # Compute Type 7 quantiles
  p            <- c((1 - level) / 2, 1 - (1 - level) / 2)
  N            <- length(delta.e)
  Q            <- sapply(p, function(p) {
    h  <- (N - 1) * p + 1
    hf <- floor(h)
    icers[hf] + (h - hf) * (icers[hf + 1] - icers[hf])
  })
  Q
}

#' Bootstrap acceptability method for confidence interval calculation
#'
#' Calculate a confidence interval using the bootstrap acceptability method by
#' manually providing incremental costs and effects. Consider using
#' \link{icerui} for greater functionality.
#'
#' @param delta.e Vector of incremental effects.
#' @param delta.c Vector of incremental costs.
#' @param level   Desired confidence level.
#' @return        A vector with the lower and upper confidence limits.
#'
#' @export
bsaccept <- function(delta.e, delta.c, level) {
  icers <- delta.c / delta.e
  wtp   <- icers[icers >= 0]
  wtp   <- wtp[order(wtp)]
  n     <- length(delta.e)
  n_ce  <- sapply(wtp, function(lambda) sum(delta.e * lambda - delta.c >= 0))
  p     <- c((1 - level) / 2, 1 - (1 - level) / 2)
  Q     <- sapply(p, function(p) {
    j <- sum(p * n > n_ce)
    if (j < length(wtp))
      wtp[j] + (p * n - floor(p * n)) * (wtp[j + 1] - wtp[j])
    else
      NA_real_
  })
  Q
}
