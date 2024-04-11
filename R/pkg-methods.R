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
#' @param fm (`string`)\cr string of unit for survival time.
print.s_survival <- function(x, fm = "months", ...) {
  cat("Surv formula: ", x$params$formula, "\n", sep = "")
  grp <- unique(x$surv$quantile$group)
  cat("Group by: ", paste(grp, collapse = ", "), "\n", sep = "")
  if (!is.null(x$params$strata)) {
    cat("Stratified by: ", paste(x$params$strata, collapse = ", "), "\n", sep = "")
  }
  cat("Confidence interval type: ", x$params$conf_type, "\n", sep = "")

  grp_var <- x$params$var
  df <- x$data
  ref_col <- grp[1]
  comp_col <- grp[-1]
  pval_name <- ifelse(x$params$rho == 0, "log-rank", "peto & peto")

  a_count_subjd <- function(df, .var, .N_col, res) {
    surv_med <- res$surv$median %>%
      mutate(
        censors = .data$n - .data$events
      ) %>%
      tibble::column_to_rownames(var = "group")
    ind <- grep(df[[.var]][1], row.names(surv_med), fixed = TRUE)
    in_rows(
      "Number of events" = rcell(
        surv_med$events[ind] * c(1, 1 / .N_col),
        format = "xx (xx.xx%)"
      ),
      "Number of consered" = rcell(
        surv_med$censors[ind] * c(1, 1 / .N_col),
        format = "xx (xx.xx%)"
      )
    )
  }

  a_surv_time_func <- function(df, .var, res) {
    med_tb <- res$surv$median %>%
      tibble::column_to_rownames(var = "group")
    quant_tb <- res$surv$quantile %>%
      tidyr::pivot_longer(cols = -c(1, 2), values_to = "Value", names_to = "Stat") %>%
      tidyr::pivot_wider(
        id_cols = "group",
        names_from = c("quantile", "Stat"),
        names_glue = "Q{quantile}_{Stat}",
        values_from = c("Value")
      ) %>%
      tibble::column_to_rownames(var = "group")
    range_tb <- res$surv$range %>%
      tibble::column_to_rownames(var = "group")
    ind <- grep(df[[.var]][1], row.names(med_tb), fixed = TRUE)
    med_time <- list(med_tb[ind, c("median", "lower", "upper")])
    quantile_time <- lapply(c(res$params$quantile * 100), function(x) {
      unlist(c(quant_tb[ind, grep(paste0("Q", x), names(quant_tb))]))
    })
    range_time <- list(range_tb[ind, c("min", "max")])
    in_rows(
      .list = c(med_time, quantile_time, range_time),
      .names = c(
        "Median (95% CI)",
        paste0(c(res$params$quantile * 100), "th percentile (95% CI)", sep = ""),
        "Min, Max"
      ),
      .formats = c(
        "xx.xx (xx.xx - xx.xx)",
        rep("xx.xx (xx.xx - xx.xx)", length(res$params$quantile)),
        "(xx.xx, xx.xx)"
      )
    )
  }

  tbl <- basic_table(
    show_colcounts = TRUE
  ) %>%
    split_cols_by(grp_var, ref_group = ref_col) %>%
    analyze(grp_var, a_count_subjd,
      show_labels = "hidden",
      extra_args = list(res = x)
    ) %>%
    analyze(grp_var, a_surv_time_func,
      var_labels = "Time to event (months)", show_labels = "visible",
      extra_args = list(res = x),
      table_names = "kmtable"
    )

  if (!is.null(x$surv_diff$test)) {
    a_surv_pval_func <- function(df, .var, .in_ref_col, res) {
      curgrp <- df[[.var]][1]
      pval_tb <- res$surv_diff$test %>%
        filter(.data$reference == ref_col & .data$comparison == curgrp)
      in_rows(
        "P-value" = non_ref_rcell(
          pval_tb[["pval"]],
          .in_ref_col,
          format = "x.xxxx | (<0.0001)"
        )
      )
    }
    tbl <- tbl %>%
      analyze(grp_var, a_surv_pval_func,
        var_labels = paste(
          ifelse(is.null(x$params$strata), "Unstratified", "Stratified"),
          paste(pval_name, "test")
        ),
        show_labels = "visible",
        extra_args = list(res = x),
        table_names = "logrank"
      )
  }

  if (!is.null(x$params$time_point)) {
    a_surv_rate_func <- function(df, .var, .in_ref_col, rate_tb, rate_diff_tb) {
      curgrp <- df[[.var]][1]
      rate_diff_tb <- rate_diff_tb %>%
        filter(.data$reference == ref_col & .data$comparison == curgrp)
      ind <- grep(df[[.var]][1], row.names(rate_tb), fixed = TRUE)
      in_rows(
        rcell(rate_tb[ind, "n.risk", drop = TRUE], format = "xx"),
        rcell(rate_tb[ind, "surv", drop = TRUE], format = "xx.xxx"),
        rcell(unlist(rate_tb[ind, c("lower", "upper"), drop = TRUE]), format = "(xx.xxx, xx.xxx)"),
        non_ref_rcell(
          rate_diff_tb[, "surv.diff", drop = TRUE],
          .in_ref_col,
          format = "xx.xxx"
        ),
        non_ref_rcell(
          unlist(rate_diff_tb[, c("lower", "upper"), drop = TRUE]),
          .in_ref_col,
          format = "(xx.xxx, xx.xxx)",
          indent_mod = 1L
        ),
        non_ref_rcell(
          rate_diff_tb[, "pval", drop = TRUE],
          .in_ref_col,
          format = "x.xxxx | (<0.0001)",
          indent_mod = 1L
        ),
        .names = c(
          "Number at risk",
          "Event-free rate", "95% CI",
          "Difference in Event Free Rate", "95% CI",
          "p-value (Z-test)"
        )
      )
    }

    time_point <- x$params$time_point
    surv_rate <- x$surv$time_point %>%
      split(as.formula("~time")) %>%
      purrr::map(\(df) tibble::column_to_rownames(df, var = "group"))
    surv_rate_diff <- x$surv_diff$rate %>%
      split(as.formula("~time"))
    for (i in seq_along(time_point)) {
      tbl <- tbl |>
        analyze(grp_var, a_surv_rate_func,
          var_labels = paste(time_point[i], fm), show_labels = "visible",
          extra_args = list(rate_tb = surv_rate[[i]], rate_diff_tb = surv_rate_diff[[i]]),
          table_names = paste0("timepoint_", time_point[i])
        )
    }
  }

  result <- tbl %>%
    build_table(df)
  print(result)

  invisible(x)
}

#' @describeIn s_get_coxph prints survival analysis summary from `coxph`.
#' @exportS3Method
#' @keywords internal
print.s_coxph <- function(x, ...) {
  cat("Surv formula: ", x$params$formula, "\n", sep = "")
  grp <- x$params$group
  cat("Group by: ", paste(grp, collapse = ", "), "\n", sep = "")
  if (!is.null(x$params$strata)) {
    cat("Stratified by: ", paste(x$params$strata, collapse = ", "), "\n", sep = "")
  }
  cat("Tie method: ", x$params$ties, "\n", sep = "")
  pval_name <- switch(x$params$pval_method,
    all = c("logtest", "sctest", "waldtest"),
    log = "logtest",
    sc = "sctest",
    wald = "waldtest"
  )
  cat("P-value method for HR: ", paste(pval_name, collapse = ", "), "\n\n", sep = "")

  grp_var <- x$params$var
  df <- x$data
  ref_col <- grp[1]

  a_hr_func <- function(df, .var, .in_ref_col, res) {
    if (.in_ref_col) {
      ret <- replicate(2 + length(pval_name), list(rcell(NULL)))
    } else {
      curgrp <- df[[.var]][1]
      hr_tb <- res$hr %>%
        filter(.data$reference == ref_col & .data$comparison == curgrp)
      pval_tb <- res$pval %>%
        filter(.data$reference == ref_col & .data$comparison == curgrp)
      ret <- list(
        non_ref_rcell(
          hr_tb[, "hr", drop = TRUE],
          .in_ref_col,
          format = "xx.xx"
        ),
        non_ref_rcell(
          unlist(hr_tb[, c("lower", "upper"), drop = TRUE]),
          .in_ref_col,
          format = "(xx.xx, xx.xx)"
        )
      )
      for (i in seq_along(pval_name)) {
        ret <- c(
          ret,
          non_ref_rcell(
            pval_tb[, "pval", drop = TRUE][i],
            .in_ref_col,
            format = "x.xxxx | (<0.0001)"
          )
        )
      }
    }
    in_rows(
      .list = ret,
      .names = c(
        "Hazard Ratio", "95% CI",
        paste0("p-value (", pval_name, ")")
      ),
      .formats = c(
        "xx.xx",
        "(xx.xx, xx.xx)",
        rep("x.xxxx | (<0.0001)", length(pval_name))
      )
    )
  }

  result <- basic_table(
    show_colcounts = TRUE
  ) %>%
    split_cols_by(grp_var, ref_group = ref_col) %>%
    analyze(grp_var, a_hr_func,
      var_labels = paste(
        ifelse(is.null(x$params$strata), "Unstratified", "Stratified"),
        "Analysis"
      ),
      show_labels = "visible",
      extra_args = list(res = x),
      table_names = "coxph"
    ) %>%
    build_table(df)
  print(result)

  invisible(x)
}
