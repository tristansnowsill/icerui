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
#' @param x       The results of a call to \code{BCEA::bcea()},
#'                \code{heemod::run_psa}, or a list with elements
#'                \code{delta.e} and \code{delta.c}.
#' @param level   The confidence level required.
#' @param method  The method to be used to calculate the uncertainty interval.
#' @param comparison  Selects the comparator, in case of more than two
#'                interventions being analysed. Uncertainty intervals will be
#'                produced for each versus \code{object$ref}
#'                (\code{confint.bcea}) or versus the first defined strategy
#'                (\code{confint.psa}). Default (NULL) results in uncertainty
#'                intervals being produced for all interventions.
#' @param ...     Additional argument(s) for methods.
#' @export
icerui <- function(x, level = 0.95,
                   method = c("fieller", "percentile", "acceptability"),
                   comparison = NULL,
                   ...) {
  stopifnot(level > 0 & level < 1)
  method <- match.arg(method)
  fun <- switch(method,
                fieller       = fieller,
                percentile    = bspercent,
                acceptability = bsaccept)

  comparison <- NULL
  if (inherits(x, "bcea")) {
    if (is.null(comparison)) comparison <- x$comp
    comparison <- extract_comparison.bcea(x, comparison)
  } else if (inherits(x, "run_psa")) {
    comparison <- extract_comparison.run_psa(x, comparison)
  } else if (all(c("delta.e", "delta.c") %in% names(x))) {

  } else {
    stop("could not construct appropriate comparison object")
  }

  interval <- compute_interval(comparison, fun, level)

  # res <- fun(delta.e, delta.c, level)
  # names(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))
  res
}

new_icerui <- function(intervals) {

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
  comparison <- extract_comparison.bcea(object, comparison)
  if (inherits(comparison, "multiple_comparison")) {
    compute_interval.multiple_comparison(comparison, fun, level)
  } else {
    compute_interval.single_comparison(comparison, fun, level)
  }
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

#' Output confidence intervals from an icerui object
#'
#' Produce a vector or matrix as would typically be produced by a call to
#' \code{confint.lm} and similar functions.
#'
#' @param x       An \code{icerui} object.
#' @return        A matrix or vector with columns giving lower and upper
#'                confidence limits for the ICER and each row giving details
#'                for an intervention. These will be labelled as (1-level)/2
#'                and 1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
#' @export
confint.icerui <- function(x) {
  stop("not yet implemented")
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

compute_interval <- function(comparison, fun, level) {
  UseMethod("compute_interval")
}

compute_interval.multiple_comparison <- function(comparison, fun, level) {
  stopifnot(inherits(comparison, "multiple_comparison"))

  res <- t(sapply(comparison, function(c) fun(c$delta.e, c$delta.c, level)))
  rownames(res) <- sapply(comparison, function(c) c$comparison)
  colnames(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))

  res
}

compute_interval.single_comparison <- function(comparison, fun, level) {
  stopifnot(inherits(comparison, "single_comparison"))

  res <- fun(comparison$delta.e, comparison$delta.c, level)
  names(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))

  res
}

extract_comparison <- function(object, comparison) {
  UseMethod("extract_comparison")
}

extract_comparison.default <- function(object, comparison) {

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
           delta.c = object$delta.c[, reindex, drop = TRUE],
           comparison = paste(object$interventions[object$ref],
                              "vs.",
                              object$interventions[i]))
    })
    class(res) <- "multiple_comparison"
  } else {
    if (object$n.comparisons > 1) {
      reindex <- which(object$comp == comparison)
      res <- list(delta.e = object$delta.e[, reindex, drop = TRUE],
                  delta.c = object$delta.c[, reindex, drop = TRUE],
                  comparison = paste(object$interventions[object$ref],
                                     "vs.",
                                     object$interventions[comparison]))
    } else {
      res <- list(delta.e = object$delta.e, delta.c = object$delta.c,
                  comparison = paste(object$interventions[object$ref],
                                     "vs.",
                                     object$interventions[comparison]))
    }
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
    comp_names <- lapply(rhs[2:length(rhs)], rlang::as_string)
    res <- mapply(function(ce, cc, name)
      list(delta.e = int_e - ce,
           delta.c = int_c - cc,
           comparison = paste(intervention,
                              "vs.",
                              name)),
      comp_e, comp_c, comp_names, SIMPLIFY = FALSE)
    class(res) <- "multiple_comparison"
  } else {
    stopifnot(rlang::is_symbol(rhs))
    s_str <- rlang::as_string(rhs)
    stopifnot(s_str %in% object$interventions)
    res <- list(delta.e = int_e - object$e[, which(object$interventions == s_str)],
                delta.c = int_c - object$c[, which(object$interventions == s_str)],
                comparison = paste(intervention, "vs.", s_str))
    class(res) <- "single_comparison"
  }

  res
}
