#' Calculate ICER uncertainty intervals
#'
#' Calculate uncertainty intervals for the incremental cost-effectiveness
#' ratio (ICER), choosing from a number of established methods.
#'
#' The Fieller method is a parametric uncertainty interval which is accurate
#' assuming incremental costs and effects follow a bivariate normal
#' distribution. The bootstrap percentile method (\code{percentile})
#' calculates ICERs for each simulation, orders these as suggested by Glick et
#' al. (2015), then takes the relevant percentiles from this vector. The
#' bootstrap acceptability method (\code{acceptability}) performs calculations
#' similarly to the cost-effectiveness acceptability curve.
#'
#' @param delta.e Vector of incremental effects.
#' @param delta.c Vector of incremental costs.
#' @param level   The confidence level required.
#' @param method  The method to be used to calculate the uncertainty interval.
#' @param ...     Additional argument(s) for methods.
#' @return        A matrix or vector with columns giving lower and upper
#'                confidence limits for the ICER and each row giving details
#'                for an intervention. These will be labelled as (1-level)/2
#'                and 1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
#' @export
icerui <- function(delta.e, delta.c, level = 0.95,
                   method = c("fieller", "percentile", "acceptability"),
                   ...) {
  stopifnot(level > 0 & level < 1)
  method <- match.arg(method)
  fun <- switch(method,
                fieller       = fieller,
                percentile    = bspercent,
                acceptability = bsaccept)
  res <- fun(delta.e, delta.c, level)
  names(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))
  res
}

#' @describeIn icerui Calculate the ICER uncertainty interval for a BCEA
#'   object
#'
#' @param object For \code{confint.bcea}, the result of a \code{BCEA::bcea()}
#'   command; for \code{confint.psa}, the result of a \code{heemod::run_psa()}
#'   command.
#' @param parm   Should not change from "ICER" (the default).
#' @param comparison  Selects the comparator, in case of more than two
#'               interventions being analysed. Uncertainty intervals will be
#'               produced for each versus \code{object$ref}
#'               (\code{confint.bcea}) or versus the first defined strategy
#'               (\code{confint.psa}). Default (NULL) results in uncertainty
#'               intervals being produced for all interventions.
#' @export
confint.bcea <- function(object, parm = "ICER", level = 0.95,
                         method = c("fieller", "percentile", "acceptability"),
                         comparison = NULL, ...) {
  stopifnot(inherits(object, "bcea"))
  stopifnot(parm == "ICER")
  stopifnot(level > 0 & level < 1)
  method <- match.arg(method)
  # Process each comparison, delegating to method-specific functions
  if (is.null(comparison)) comparison <- object$comp
  fun <- switch(method,
                fieller       = fieller,
                percentile    = bspercent,
                acceptability = bsaccept)
  if (length(comparison) > 1) {
    inner <- function(index, fun) {
      reindex <- which(object$comp == index)
      fun(object$delta.e[, reindex], object$delta.c[, reindex], level)
    }
    res <- t(sapply(comparison, inner, fun = fun))
    rownames(res) <- object$interventions[comparison]
    colnames(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))
  } else {
    if (object$n.comparisons > 1) {
      reindex <- which(object$comp == comparison)
      res <- fun(object$delta.e[, reindex], object$delta.c[, reindex], level)
    } else {
      res <- fun(object$delta.e, object$delta.c, level)
    }
    names(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))
  }
  res
}

#' @describeIn icerui Calculate the ICER uncertainty interval for a heemod PSA
#'   object
#' @export
confint.psa <- function(object, parm = "ICER", level = 0.95,
                        method = c("fieller", "percentile", "acceptability"),
                        comparison = NULL, ...) {
  stopifnot(inherits(object, "psa"))
  # Big lazy
  if (requireNamespace("heemod", quietly = TRUE) & requireNamespace("BCEA", quietly = TRUE)) {
    confint.bcea(heemod::run_bcea(object), level, method, comparison, ...)
  } else {
    stop("heemod and/or BCEA package is not installed.")
  }
}

#' Cost-effectiveness scatter plot with confidence intervals
#'
#' Produces a scatter plot with incremental effects on the x-axis and
#' incremental costs on the y-axis, accompanied by the confidence intervals
#' produced by the selected method. Can use base or ggplot2 graphics (if
#' installed).
#'
#' @param object  A \code{bcea} or \code{psa} object.
#' @param delta.e A vector of incremental effects.
#' @param delta.c A vector of incremental costs.
#' @param level   The confidence level required.
#' @param method  The method to be used to calculate the uncertainty interval
#'                (see \link{icerui}).
#' @param comparison  Select one comparator (by index or name if \code{object}
#'   is a \code{bcea} object, or by name if \code{object} is a \code{psa}
#'   object).
#' @param graph   Specify the graphics engine to use.
#' @param ...     Additional argument(s) for methods.
#' @export
uiplot <- function(object = NULL, delta.e = NULL, delta.c = NULL, level = 0.95,
                   method = c("fieller", "percentile", "acceptability"),
                   comparison = NULL, graph = c("base", "ggplot2"), ...) {
  if (!xor(is.null(delta.e) | is.null(delta.c), is.null(object))) {
    stop("Must specify EITHER delta.e and delta.c OR object.")
  }

  stopifnot(is.null(object) | inherits(object, "bcea") | inherits(object, "psa"))

  if (!is.null(object) & inherits(object, "bcea")) {
    if (is.null(comparison)) comparison <- object$comp
    stopifnot(length(comparison) == 1)
    if (object$n.comparisons > 1) {
      if (is.numeric(comparison)) {
        reindex <- which(object$comp == comparison)
      } else {
        reindex <- which(object$interventions[object$comp] == comparison)
      }
      delta.e <- object$delta.e[, reindex]
      delta.c <- object$delta.c[, reindex]
    } else {
      delta.e <- object$delta.e
      delta.c <- object$delta.c
    }
  }

  if (!is.null(object) & inherits(object, "psa")) {

  }

  method <- match.arg(method)
  ui <- icerui(delta.e, delta.c, level = level, method = method)

  graph <- match.arg(graph)
  if (graph == "ggplot2") {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot2::ggplot(data.frame(x = delta.e, y = delta.c),
                           ggplot2::aes(x = e, y = c)) +
        ggplot2::geom_point() +
        ggplot2::geom_abline(slope = ui[1], linetype = "dashed") +
        ggplot2::geom_abline(slope = ui[2], linetype = "dashed") +
        ggplot2::expand_limits(x = 0, y = 0) +
        ggplot2::labs(x = "Incremental effectiveness", y = "Incremental costs")
      return(p)
    } else {
      stop("ggplot2 is not installed. Please install it or use an alternative.")
    }
  }
  if (graph == "base") {
    extent <- function(d) c(min(d), max(d))
    expanded_limits <- function(lim, include = 0) c(min(lim[1], include), max(lim[2], include))
    graphics::plot(
      delta.e,
      delta.c,
      xlim = expanded_limits(extent(delta.e)),
      ylim = expanded_limits(extent(delta.c)),
      xlab = "Incremental effectiveness",
      ylab = "Incremental costs",
      pch  = 21,
      bg   = "grey")
    graphics::abline(a = 0, b = ui[1], lty = 2)
    graphics::abline(a = 0, b = ui[2], lty = 2)
  }
}

extract_comparison.bcea <- function(object, comparison) {
  # If there is only one comparison this will return a single_comparison
  # object. If there is more than one comparison it will return a
  # multiple_comparison object.

  # Check what type of comparison object has been provided and delegate
  stopifnot(inherits(object, "bcea"))

  if (is.numeric(comparison)) {
    extract_comparison_index.bcea(object, comparison)
  } else if (is.character(comparison)) {
    extract_comparison_names.bcea(object, comparison)
  } else if (rlang::is_formula(comparison)) {
    extract_comparison_formula.bcea(object, comparison)
  }
}

extract_comparison_index.bcea <- function(object, comparison) {
  res <- NULL
  if (length(comparison) > 1) {
    res <- lapply(comparison, function(i) {
      reindex <- which(object$comp == i)
      list(delta.e = object$delta.e[, reindex, drop = TRUE],
           delta.c = object$delta.c[, reindex, drop = TRUE])
    })
    class(res) <- "multiple_comparison"
  } else {
    res <- list(delta.e = object$delta.e[, reindex, drop = TRUE],
                delta.c = object$delta.c[, reindex, drop = TRUE])
    class(res) <- "single_comparison"
  }
  res
}

extract_comparison_names.bcea <- function(object, comparison) {
  comparison <- sapply(comparison,
                       function(x) which(object$interventions[object$comp] == x))
  extract_comparison_index.bcea(object, comparison)
}

extract_comparison_formula.bcea <- function(object, comparison) {
  # Check we have a single intervention on LHS and retrieve costs and effects
  stopifnot(rlang::is_symbol(lhs <- rlang::f_lhs(comparison)))
  intervention <- rlang::as_string(lhs)
  stopifnot(intervention %in% object$interventions)
  int_e <- object$e[, which(object$interventions == intervention)]
  int_c <- object$c[, which(object$interventions == intervention)]

  res <- NULL

  # Check whether RHS is single or multiple comparators
  if (length(rhs <- rlang::f_rhs(comparison)) > 1) {
    # Retrieve costs and effects for multiple comparators
    stopifnot("multiple comparators must be specified as list(comp1, comp2, ...)" = rhs[[1]] == "list")
    comp_e <- lapply(rhs[2:length(rhs)], function(s) {
      stopifnot(rlang::is_symbol(s))
      s_str <- rlang::as_string(s)
      stopifnot(s_str %in% object$interventions)
      object$e[, which(object$interventions == s_str)]
    })
    comp_c <- lapply(rhs[2:length(rhs)],
                     function(s) object$c[, which(object$interventions == rlang::as_string(s))])
    res <- mapply(function(ce, cc) list(delta.e = int_e - ce, delta.c = int_c - cc),
                  comp_e, comp_c, SIMPLIFY = FALSE)
    class(res) <- "multiple_comparison"
  } else {
    stopifnot(rlang::is_symbol(rhs))
    s_str <- rlang::as_string(rhs)
    stopifnot(s_str %in% object$interventions)
    res <- list(delta.e = int_e - object$e[, which(object$interventions == s_str)],
                delta.c = int_c - object$c[, which(object$interventions == s_str)])
    class(res) <- "single_comparison"
  }

  res
}

fieller <- function(delta.e, delta.c, level) {
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
