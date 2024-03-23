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


#' @describeIn s_get_survfit prints survival analysis summary from `survfit`.
#' @exportS3Method
#' @keywords internal
print.s_survival <- function(x, ...) {
  cat("Surv formula: ", x$params$formula, "\n", sep = "")
  cat("Group by: ", paste(unique(x$surv$quantile$group), collapse = ", "), "\n", sep = "")
  if (!is.null(x$params$strata)) {
    cat("Stratified by: ", paste(x$params$strata, collapse = ", "), "\n", sep = "")
  }
  cat("Confidence interval type: ", x$params$conf_type, "\n", sep = "")

  cat("\n---\n")
  cat("Estimation of Survival Time:\n")
  med <- x$surv$median %>%
    rowwise() %>%
    mutate(
      `n(event)` = format_value(c(.data$n, .data$events), format = "xx (xx)"),
      Median = format_value(c(.data$median, .data$lower, .data$upper),
        format = "xx.xx (xx.xx - xx.xx)"
      )
    ) %>%
    select("group", "n(event)", "Median")
  quant <- x$surv$quantile %>%
    rowwise() %>%
    mutate(
      surv = format_value(c(.data$time, .data$lower, .data$upper),
        format = "xx.xx (xx.xx - xx.xx)"
      )
    ) %>%
    tidyr::pivot_wider(
      id_cols = "group",
      names_from = "quantile",
      names_glue = "Q{quantile}",
      values_from = "surv"
    )
  rang <- x$surv$range %>%
    rowwise() %>%
    mutate(
      `Range (event)` = format_value(c(.data$event_min, .data$event_max),
        format = "(xx.x, xx.x)"
      ),
      `Range (censor)` = format_value(c(.data$censor_min, .data$censor_max),
        format = "(xx.x, xx.x)"
      ),
      `Range` = format_value(c(.data$min, .data$max),
        format = "(xx.x, xx.x)"
      )
    ) %>%
    select("group", "Range (event)", "Range (censor)", "Range")
  list(med, quant, rang) %>%
    purrr::reduce(full_join, by = "group") %>%
    tidyr::pivot_longer(cols = -1, values_to = "Value", names_to = "Stat") %>%
    tidyr::pivot_wider(names_from = "group", values_from = "Value") %>%
    tibble::column_to_rownames(var = "Stat") %>%
    print()
  cat("\n")


  if (!is.null(x$surv$time_point)) {
    cat("---\n")
    cat(
      "Survival Time at Specified Time Points (",
      paste(unique(x$surv$time_point$time), collapse = ","), "):\n",
      sep = ""
    )
    # print(x$surv$time_point)
    x$surv$time_point %>%
      rowwise() %>%
      mutate(
        `Number at risk` = as.character(.data$n.risk),
        `Number of event` = as.character(.data$n.event),
        `Number of consor` = as.character(.data$n.censor),
        `Survival Rate` = format_value(c(.data$surv, .data$lower, .data$upper),
          format = "xx.xx (xx.xx - xx.xx)"
        )
      ) %>%
      select(
        "time", "Number at risk", "Number of event",
        "Number of consor", "Survival Rate", "group"
      ) %>%
      ungroup() %>%
      dplyr::group_split(.data$time) %>%
      purrr::walk(.f = function(dt) {
        dt %>%
          select(-1) %>%
          tidyr::pivot_longer(cols = -c("group"), values_to = "Value", names_to = "Stat") %>%
          tidyr::pivot_wider(names_from = "group", values_from = "Value") %>%
          tibble::column_to_rownames(var = "Stat") %>%
          print()
        cat("\n")
      })
  }

  if (!is.null(x$surv_diff$rate)) {
    cat("---\n")
    cat(
      "Survival Difference at Specified Time Points (",
      paste(unique(x$surv$time_point$time), collapse = ","), "):\n",
      sep = ""
    )
    x$surv_diff$rate %>%
      rowwise() %>%
      mutate(
        `Diff (Survival Rate)` = format_value(c(.data$surv.diff, .data$lower, .data$upper),
          format = "xx.xx (xx.xx - xx.xx)"
        ),
        `p-value` = format_value(.data$pval, "x.xxxx | (<0.0001)")
      ) %>%
      select(
        "time", "Diff (Survival Rate)", "p-value", "group"
      ) %>%
      ungroup() %>%
      dplyr::group_split(.data$time) %>%
      purrr::walk(.f = function(dt) {
        dt %>%
          select(-1) %>%
          tidyr::pivot_longer(cols = -c("group"), values_to = "Value", names_to = "Stat") %>%
          tidyr::pivot_wider(names_from = "group", values_from = "Value") %>%
          tibble::column_to_rownames(var = "Stat") %>%
          print()
        cat("\n")
      })
  }

  method <- if (x$params$rho == 0) {
    "Log-Rank"
  } else if (x$params$rho == 1) {
    "Peto & Peto"
  }
  if (!is.null(x$surv_diff$test)) {
    cat("---\n")
    cat("Hypothesis Testing with ", method, ":\n", sep = "")
    x$surv_diff$test %>%
      tidyr::pivot_wider(names_from = "comparsion", values_from = "pval") %>%
      tibble::column_to_rownames(var = "method") %>%
      print()
  }

  invisible(x)
}

#' @describeIn s_get_coxph prints survival analysis summary from `coxph`.
#' @exportS3Method
#' @keywords internal
print.s_coxph <- function(x, ...) {
  cat("Surv formula: ", x$params$formula, "\n", sep = "")
  cat("Group by: ", paste(x$params$group, collapse = ", "), "\n", sep = "")
  if (!is.null(x$params$strata)) {
    cat("Stratified by: ", paste(x$params$strata, collapse = ", "), "\n", sep = "")
  }
  cat("Tie method: ", x$params$ties, "\n", sep = "")
  cat("P-value method for HR: ",
    switch(x$params$pval_method,
      all = paste(c("logtest", "sctest", "waldtest"), collapse = ", "),
      log = "logtest",
      sc = "sctest",
      wald = "waldtest"
    ),
    "\n",
    sep = ""
  )

  cat("\n---\n")
  cat("Estimation of Hazard Ratio",
    ifelse(!is.null(x$params$strata), " with Stratification", ""), ":\n",
    sep = ""
  )
  hr <- x$hr %>%
    rowwise() %>%
    mutate(
      `n(event)` = format_value(c(.data$n, .data$events), format = "xx (xx)"),
      `Hazard Ratio` = format_value(c(.data$hr, .data$lower, .data$upper),
        format = "xx.xx (xx.xx - xx.xx)"
      )
    ) %>%
    select("comparsion", "n(event)", "Hazard Ratio")
  pval <- x$pval %>%
    rowwise() %>%
    mutate(
      pval = format_value(.data$pval, "x.xxxx | (<0.0001)")
    ) %>%
    select("comparsion", "method", "pval") %>%
    tidyr::pivot_wider(
      id_cols = "comparsion",
      names_from = "method",
      names_glue = "p-value ({method})",
      values_from = "pval"
    )

  list(hr, pval) %>%
    purrr::reduce(full_join, by = "comparsion") %>%
    tidyr::pivot_longer(cols = -1, values_to = "Value", names_to = "Stat") %>%
    tidyr::pivot_wider(names_from = "comparsion", values_from = "Value") %>%
    tibble::column_to_rownames(var = "Stat") %>%
    print()

  invisible(x)
}
