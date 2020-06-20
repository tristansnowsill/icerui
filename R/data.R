#' BCEA object for cost-effectiveness of smoking cessation interventions
#'
#' An object of class \code{bcea} derived from the dataset \link{Smoking}
#' @source \url{http://www.statistica.it/gianluca/software/bcea/}
"ex_smoking"

#' Artificial BCEA object with two options generated from multivariate normal
#' distribution using `MASS::mvrnorm`
#'
#' The dataset is constructed such that the mean costs are 5000 and 10000
#' and mean effects are 2.5 and 2.7. Incremental costs and effects have
#' standard deviations 1000 * sqrt(3) and 0.2 and are negatively correlated
#' (\eqn{rho = -0.2 * sqrt(3) \approx 0.3464}).
"ex_simple"
