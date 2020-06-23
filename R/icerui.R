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

  comparison <- extract_comparison(x, comparison)

  interval <- compute_interval(comparison, fun, level)

  # res <- fun(delta.e, delta.c, level)
  # names(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))
  # interval

  if (inherits(comparison, "multiple_comparison")) {
    validate_icerui(
      new_icerui(
        comparison,
        apply(interval, 1, function(i) list(interval = i, method = method, level = level) )))
  } else {
    validate_icerui(
      new_icerui(
        comparison,
        list(list(interval = interval, method = method, level = level))))
  }

}

new_icerui <- function(comparison, interval) {
  if (inherits(comparison, "single_comparison")) {
    structure(
      list(comparison = comparison, interval = interval),
      n.comparisons = 1,
      class = "icerui"
    )
  } else if (inherits(comparison, "multiple_comparison")) {
    structure(
      list(comparison = comparison, interval = interval),
      n.comparisons = length(comparison),
      class = "icerui")
  } else {
    stop("cannot construct icerui object from ", comparison)
  }
}

validate_icerui <- function(x) {
  # TODO: Some validation

  x
}

#' @export
`[.icerui` <- function(x, i) {
  stopifnot(i <= attr(x, "n.comparisons"))
  if (attr(x, "n.comparisons") == 1) {
    validate_icerui(new_icerui(x$comparison, x$interval[[1]]))
  } else {
    validate_icerui(new_icerui(x$comparison[[i]], x$interval[[i]]))
  }
}

#' @export
print.icerui <- function(x, ...) {
  NextMethod()
}

#' @export
plot.icerui <- function(x, comp = NULL, graph = c("base", "ggplot2"), ...) {
  graph <- match.arg(graph)
  if (is.null(comp)) comp <- 1:attr(x, "n.comparisons")
  if (length(comp) > 1) {
    plot_multiple_icerui(x, comp, graph, ...)
  } else {
    stopifnot(comp <= attr(x, "n.comparisons"))
    plot_single_icerui(x[comp], graph, ...)
  }
}

plot_multiple_icerui <- function(x, comp, graph, ...) {
  if (graph == "base") {
    n_tiles <- length(comp)
    n_cols <- ceiling(sqrt(n_tiles))
    n_rows <- ceiling(n_tiles / n_cols)
    par(mfrow = c(n_rows, n_cols))
    # cols <- ((0:(n_tiles - 1)) %% n_cols) + 1
    # rows <- (1:n_tiles) - (rows * n_cols)
    extents.e <- lapply(comp, function(c) {
      c(min(x[c]$comparison$delta.e), max(x[c]$comparison$delta.e))
    })
    extents.e <- Reduce(function (accumulated, item) c(min(accumulated[1], item[1]), max(accumulated[2], item[2])), extents.e, c(0, 0))
    extents.c <- lapply(comp, function(c) {
      c(min(x[c]$comparison$delta.c), max(x[c]$comparison$delta.c))
    })
    extents.c <- Reduce(function (accumulated, item) c(min(accumulated[1], item[1]), max(accumulated[2], item[2])), extents.c, c(0, 0))
    for (c in comp) {
      graphics::plot(x[c]$comparison$delta.e, x[c]$comparison$delta.c,
                     main = x[c]$comparison$comparison,
                     xlim = extents.e,
                     ylim = extents.c,
                     xlab = "Incremental effectiveness",
                     ylab = "Incremental costs")
      if (!is.na(x[c]$interval$interval[1])) graphics::abline(a = 0, b = x[c]$interval$interval[1], lty = 2)
      if (!is.na(x[c]$interval$interval[2])) graphics::abline(a = 0, b = x[c]$interval$interval[2], lty = 2)
    }
  } else if (graph == "ggplot2") {
    if (requireNamespace("ggplot2", quietly = TRUE)) {

    } else {
      stop("please install ggplot2 or use graph = \"base\"")
    }
  } else {
    stop("graph type ", graph, " not supported")
  }
}

plot_single_icerui <- function(x, graph, ...) {
  if (graph == "base") {
    par(mfrow = c(1, 1))
    graphics::plot(x$comparison$delta.e, x$comparison$delta.c,
                   main = x$comparison$comparison,
                   xlim = c(min(x$comparison$delta.e, 0), max(x$comparison$delta.e, 0)),
                   ylim = c(min(x$comparison$delta.c, 0), max(x$comparison$delta.c, 0)),
                   xlab = "Incremental effectiveness",
                   ylab = "Incremental costs")
    if (!is.na(x$interval$interval[1])) graphics::abline(a = 0, b = x$interval$interval[1], lty = 2)
    if (!is.na(x$interval$interval[2])) graphics::abline(a = 0, b = x$interval$interval[2], lty = 2)
  } else if (graph == "ggplot2") {
    if (requireNamespace("ggplot2", quietly = TRUE)) {

    } else {
      stop("please install ggplot2 or use graph = \"base\"")
    }
  } else {
    stop("graph type ", graph, " not supported")
  }
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
#' @param object  An \code{icerui} object.
#' @param parm    Should not change from the default ("ICER")
#' @param level   Required confidence level.
#' @return        A matrix or vector with columns giving lower and upper
#'                confidence limits for the ICER and each row giving details
#'                for an intervention. These will be labelled as (1-level)/2
#'                and 1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
#' @export
confint.icerui <- function(object, parm = "ICER", level, ...) {
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
#' @importFrom rlang .data
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
                           ggplot2::aes(x = .data$e, y = .data$c)) +
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

#' @export
extract_comparison <- function(object, comparison) {
  UseMethod("extract_comparison")
}

#' @export
extract_comparison.default <- function(object, comparison) {
  stop("function not implemented")
}

#' @export
extract_comparison.psa <- function(object, comparison) {
  stopifnot(inherits(object, "psa"))

  if (is.character(comparison)) {
    extract_comparison_names.psa(object, comparison)
  } else if (rlang::is_formula(comparison)) {
    extract_comparison_formula.psa(object, comparison)
  } else {
    stop("cannot extract comparison ", comparison)
  }
}

extract_comparison_names.psa <- function(object, comparison) {
  psa_names <- unique(object$psa$.strategy_names)
  ref <- object$model$central_strategy
  if (length(comparison) > 1) {
    stopifnot(all(comparison %in% psa_names))
    if (any(comparison == ref)) stop("cannot include ", ref, " in comparison because it is the central_Strategy")
    ref.c <- with(object$psa, .cost[.strategy_names == ref])
    ref.e <- with(object$psa, .effect[.strategy_names == ref])
    validate_multiple_comparison(
      new_multiple_comparison(
        lapply(comparison, function(x) {
          list(delta.e = with(object$psa, .cost[.strategy_names == x]) - ref.e,
               delta.c = with(object$psa, .effect[.strategy_names == x]) - ref.c,
               comparison = paste(x, "vs.", ref))
        })
      )
    )
  } else {
    stopifnot(comparison %in% psa_names)
    if (comparison == ref) stop("cannot specify ", comparison, " as comparator because it is the central_strategy")
    intervention.c <- with(object$psa, .cost[.strategy_names == comparison])
    intervention.e <- with(object$psa, .effect[.strategy_names == comparison])
    ref.c <- with(object$psa, .cost[.strategy_names == ref])
    ref.e <- with(object$psa, .effect[.strategy_names == ref])
    delta.c <- intervention.c - ref.c
    delta.e <- intervention.e - ref.e
    validate_single_comparison(
      new_single_comparison(
        list(delta.e = delta.e,
             delta.c = delta.c,
             comparison = paste(comparison, "vs.", ref))))
  }
}

#' @export
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
  } else {
    stop("cannot extract comparison ", comparison)
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
    res <- validate_multiple_comparison(new_multiple_comparison(res))
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
    res <- validate_single_comparison(new_single_comparison(res))
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
    res <- validate_multiple_comparison(new_multiple_comparison(res))
  } else {
    stopifnot(rlang::is_symbol(rhs))
    s_str <- rlang::as_string(rhs)
    stopifnot(s_str %in% object$interventions)
    res <- list(delta.e = int_e - object$e[, which(object$interventions == s_str)],
                delta.c = int_c - object$c[, which(object$interventions == s_str)],
                comparison = paste(intervention, "vs.", s_str))
    res <- validate_single_comparison(new_single_comparison(res))
  }

  res
}
