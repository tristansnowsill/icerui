# There are two types of comparison, single_comparison and
# multiple_comparison. multiple_comparison aggregates multiple
# single_comparison objects.

new_single_comparison <- function(.l) {
  structure(.l, class = "single_comparison")
}

validate_single_comparison <- function(x) {
  stopifnot(is.null(dim(x$delta.e)))
  stopifnot(is.null(dim(x$delta.c)))
  stopifnot(length(x$delta.e) == length(x$delta.c))

  x
}

#' @export
print.single_comparison <- function(x, ...) {
  # Do some printing
  cat("Single cost-effectiveness comparison of", x$comparison)
  cat("\n\nNumber of cost-effectiveness data points: ",
      length(x$delta.e),
      if (any(is.na(x$delta.e) | is.na(x$delta.c))) {
        paste0("(", sum( apply(cbind(x$delta.e, x$delta.c), 1, function(.) any(is.na(.))) ), " missing)")
      } else "",
      sep = "")
  cat("\nMean incremental cost:", mean(x$delta.c))
  cat("\nMean incremental effect:", mean(x$delta.e))
  cat("\nIncremental cost-effectiveness ratio (means):", mean(x$delta.c) / mean(x$delta.e))

  # Return invisibly
  invisible(x)
}


new_multiple_comparison <- function(.l) {
  structure(
    lapply(.l, function(x) validate_single_comparison(new_single_comparison(x))),
    class = "multiple_comparison", n.comparisons = length(.l))
}

validate_multiple_comparison <- function(x) {
  stopifnot(length(x) == attr(x, "n.comparisons"))

  delta_e_lengths <- sapply(x, function(comp) length(comp$delta.e))
  delta_c_lengths <- sapply(x, function(comp) length(comp$delta.c))

  stopifnot(all(delta_e_lengths == delta_e_lengths[1]))

  x
}

#' @export
`[.multiple_comparison` <- function(x, i) {
  NextMethod()
}

#' @export
print.multiple_comparison <- function(x, ...) {
  # Do some printing
  cat(attr(x, "n.comparisons"), "cost-effectiveness comparisons")
  cat("\n\nNumber of cost-effectiveness data points:",
      length(x[[1]]$delta.e), "\n\n")

  outtable <- data.frame(
    `Inc.Costs` = sapply(x, function(.) mean(.$delta.c)),
    `Inc.Effects` = sapply(x, function(.) mean(.$delta.e)))
  outtable$ICER <- outtable$Inc.Costs / outtable$Inc.Effects
  rownames(outtable) <- sapply(x, function(.) .$comparison)
  # cat("\nMean incremental cost:", mean(x$delta.c))
  # cat("\nMean incremental effect:", mean(x$delta.e))
  # cat("\nIncremental cost-effectiveness ratio (means):", mean(x$delta.c) / mean(x$delta.e))
  print(outtable)

  # Return invisibly
  invisible(x)
}


#' @export
compute_interval <- function(comparison, fun, level) {
  UseMethod("compute_interval")
}

#' @export
compute_interval.multiple_comparison <- function(comparison, fun, level) {
  stopifnot(inherits(comparison, "multiple_comparison"))

  res <- t(sapply(comparison, function(c) fun(c$delta.e, c$delta.c, level)))
  rownames(res) <- sapply(comparison, function(c) c$comparison)
  colnames(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))

  res
}

#' @export
compute_interval.single_comparison <- function(comparison, fun, level) {
  stopifnot(inherits(comparison, "single_comparison"))

  res <- fun(comparison$delta.e, comparison$delta.c, level)
  names(res) <- sprintf("%.1f%%", 100 * c((1 - level) / 2, 1 - (1 - level) / 2))

  res
}
