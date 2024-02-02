#' @describeIn s_get_lsmeans prints Least-squares Means summary.
#' @exportS3Method
#' @keywords internal
print.s_lsmeans <- function(x, ...) {
  cat("Model Call: ", append = FALSE)
  print(x$model$call)
  cat("\n")
  varc <- paste0(x$model$xlev[[x$params$var]], collapse = ", ")
  cat("Predictor/Treatment: ", x$params$var, " (", varc, ")\n", sep = "")
  if (!is.null(x$params$by)) {
    byc <- paste0(x$model$xlev[[x$params$by]], collapse = ", ")
    cat("Group by: ", x$params$by, " (", byc, ")\n", sep = "")
  }

  cat("\n")
  cat("Least-squares Means Estimates:\n")
  print(x$lsm_est)

  if (x$params$contrast) {
    cat("\n")
    cat("Contrast Estimates of Least-squares Means:\n")
    if (x$params$alternative == "two.sided") {
      cat(sprintf("Null hypothesis is \u03b8 equal to %s.\n", x$params$null))
    } else if (x$params$alternative == "greater" & x$params$null >= 0) {
      cat(sprintf("Null hypothesis is \u03b8 non-superiority to %s.\n", x$params$null))
    } else if (x$params$alternative == "greater" & x$params$null < 0) {
      cat(sprintf("Null hypothesis is \u03b8 inferiority to %s.\n", x$params$null))
    } else if (x$params$alternative == "less" & x$params$null >= 0) {
      cat(sprintf("Null hypothesis is \u03b8 superiority to %s.\n", x$params$null))
    }
    print(x$lsm_contr)
  }

  invisible(x)
}

#' @describeIn prop_odds_ratio prints proportion and confidence interval.
#' @exportS3Method
#' @keywords internal
print.prop_ci <- function(x, ...) {
  cat(sprintf("Proportion and %s confidence interval:", x$params$method))
  cat("\n")
  print(x$prop_est)

  if (!is.null(x$params$by.level)) {
    cat(sprintf("\nProportion Difference and %s confidence interval:", x$params$diff.method))
    cat("\n")
    print(x$prop_diff)
  }

  invisible(x)
}

#' @describeIn prop_odds_ratio prints odds ratio and confidence interval.
#' @exportS3Method
#' @keywords internal
print.or_ci <- function(x, ...) {
  comp <- paste0(rev(x$params$by.level), collapse = "/")
  cat(sprintf("Common Odds Ratio (%s) and %s confidence interval:", comp, x$params$or.method))
  cat("\n")
  print(x$or)

  if (!is.null(x$params$strata)) {
    cat(sprintf(
      "\nStratified Odds Ratio (%s) using %s", comp,
      ifelse(x$params$strata.method == "CMH",
        "Cochran-Mantel-Haenszel Chi-Squared Test:",
        "Conditional logistic regression:"
      )
    ))
    cat("\n")
    print(x$strata_or)
  }

  invisible(x)
}
