# s_get_survfit ----

#' Summarize Survival Curves Analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function summarizes Kaplan-Meier model and provides the corresponding
#' statistical outputs, such as median and percentile survival time, survival
#' time range, testing survival curve differences with or without stratification,
#' time-point survival rates and difference of survival rates.
#'
#' @param data (`data.frame`)\cr a data frame as input.
#' @param formula (`formula`)\cr a formula of survival model with survival object.
#'  See more details in [survival::survfit.formula()].
#' @param quantile (`numeric`)\cr a numeric vector specifying the quantile of
#'  survival time, default values are the 0.25 and 0.75 quantiles.
#' @param time_point (`numeric`)\cr a numeric vector representing the survival
#'  time point of interest.
#' @param conf_type (`string`)\cr string specifying the confidence interval for
#'  survival curve, default is log-log. See more details in [survival::survfit.formula()].
#' @param conf_level (`numeric`)\cr significance level for a two-side confidence
#'  interval on survival curve, default is 0.95.
#' @param strata (`character`)\cr a character vector used for stratification.
#' @param pval_method (`string`)\cr method of comparing survival curves. Default is
#'  "log-rank", others options can see [survminer::surv_pvalue()].
#' @param pairwise (`logical`)\cr whether to conduct the pairwise comparison.
#' @param ... other arguments to be passed to [survival::survfit()].
#'
#' @return
#' An object of class `s_survival` is a list contains several summary tables
#'
#' - `surv` list of median and quantiles survival time and CI, overall and time point
#' survival rates, and range of survival time.
#' - `surv_diff` list of p-value from `survfit` object comparing survival curves, and
#' the difference of survival rates.
#' - `params` essential parameters.
#'
#' @note
#' This function mainly wraps `survival::survfit()` to compute the survival
#' estimates, and `survminer::surv_pvalue()` to compute the p-value of comparing
#' survival curves. The SE and p-value of difference rate are simply computed
#' using normal distribution and Z statistic.
#'
#' Stratification can only alter the testing survival curves, therefore there will
#' be no changes to other outputs when the `strata` argument is defined.
#'
#' @export
#'
#' @examples
#' data("whas500")
#'
#' # reorder the grouping variable
#' dat <- whas500 %>%
#'   mutate(
#'     AFB = factor(AFB, levels = c(1, 0))
#'   )
#'
#' # no grouping
#' s_get_survfit(data = dat, formula = Surv(LENFOL, FSTAT) ~ 1)
#'
#' # grouping by AFB
#' s_get_survfit(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)
#'
#' # specify the time points for survival rate, and AFB=1 as the reference
#' s_get_survfit(
#'   data = dat,
#'   formula = Surv(LENFOL, FSTAT) ~ AFB,
#'   time_point = c(12, 36, 60)
#' )
#'
#' # specify the stratified log-rank test
#' s_get_survfit(
#'   data = dat,
#'   formula = Surv(LENFOL, FSTAT) ~ AFB,
#'   strata = c("AGE", "GENDER")
#' )
#'
#' # dummy three groups
#' set.seed(123)
#' subj <- sample(dat$ID, 100)
#' dat2 <- whas500 %>%
#'   mutate(
#'     AFB = case_when(
#'       ID %in% subj ~ 2,
#'       TRUE ~ AFB
#'     ),
#'     AFB = factor(AFB, levels = c(1, 2, 0))
#'   )
#'
#' # pairwise comparison
#' s_get_survfit(
#'   data = dat2,
#'   formula = Surv(LENFOL, FSTAT) ~ AFB,
#'   time_point = c(12, 36, 60),
#'   pairwise = TRUE
#' )
s_get_survfit <- function(data,
                          formula,
                          quantile = c(0.25, 0.75),
                          time_point = NULL,
                          conf_type = c("log-log", "log", "plain"),
                          conf_level = 0.95,
                          strata = NULL,
                          pval_method = "log-rank",
                          pairwise = FALSE,
                          ...) {
  assert_class(data, "data.frame")
  assert_formula(formula)
  assert_numeric(quantile, lower = 0, upper = 1)
  assert_numeric(time_point, null.ok = TRUE)
  assert_number(conf_level, lower = 0, upper = 1)
  assert_subset(strata, names(data))
  assert_string(pval_method)
  assert_logical(pairwise)
  conf_type <- match.arg(conf_type, c("log-log", "log", "plain"), several.ok = FALSE)

  km_fit <- survival::survfit(
    data = data,
    formula = formula,
    conf.int = conf_level,
    conf.type = conf_type,
    ...
  )

  grps <- if (is.null(km_fit$strata)) {
    "Total"
  } else {
    names(km_fit$strata)
  }

  # quantile survival time
  quant <- quantile(km_fit, probs = quantile) %>%
    purrr::map(function(x) {
      if (is.null(km_fit$strata)) {
        as.data.frame(x) %>% set_colnames(value = "Total")
      } else {
        as.data.frame(t(x))
      }
    }) %>%
    purrr::list_rbind() %>%
    tidyr::pivot_longer(cols = dplyr::all_of(grps), names_to = "group") %>%
    mutate(
      type = rep(c("time", "lower", "upper"), each = length(quantile) * length(grps)),
      quantile = rep(quantile * 100, each = length(grps), times = 3)
    ) %>%
    tidyr::pivot_wider(names_from = type, values_from = value)

  # median survival time
  surv_tb <- if (is.null(km_fit$strata)) {
    broom::glance(km_fit) %>%
      select(n = nobs, events, median, lower = conf.low, upper = conf.high)
  } else {
    tibble::as_tibble(summary(km_fit)$table) %>%
      mutate(group = grps) %>%
      select(group, n = n.start, events, median, lower = `0.95LCL`, upper = `0.95UCL`)
  }

  # test survival curves
  surv_test <- if (!is.null(km_fit$strata)) {
    if (is.null(strata)) {
      km_no_strata <- do.call(
        survival::survfit,
        args = list(
          data = data,
          formula = formula,
          conf.int = conf_level,
          conf.type = conf_type
        )
      )
      survminer::surv_pvalue(km_no_strata, data = data, method = pval_method) %>%
        tibble::as_tibble()
    } else {
      formula_strata <- as.formula(
        paste0(format(formula), " + strata(", paste(strata, collapse = ", "), ")")
      )
      km_strata <- do.call(
        survival::survfit,
        args = list(
          data = data,
          formula = formula_strata,
          conf.int = conf_level,
          conf.type = conf_type
        )
      )
      survminer::surv_pvalue(km_strata, data = data, method = pval_method) %>%
        tibble::as_tibble()
    }
  }

  # overall survival rate
  cols <- c("time", "n.risk", "n.event", "n.censor", "surv", "std.err", "lower", "upper")
  overall <- summary(km_fit)
  surv_overall_rate <- if (is.null(km_fit$strata)) {
    tibble::as_tibble(overall[cols]) %>%
      mutate(group = grps)
  } else {
    evt_ind <- which(overall$time < lag(overall$time)) - 1
    evt_ind <- c(evt_ind[1], diff(c(evt_ind, length(overall$time))))
    tibble::as_tibble(overall[cols]) %>%
      mutate(group = rep(grps, times = evt_ind))
  }

  # time-point survival rate
  surv_tp_rate <- if (!is.null(time_point)) {
    summary(km_fit, times = time_point, extend = TRUE)[cols] %>%
      tibble::as_tibble() %>%
      mutate(
        group = rep(grps, each = length(time_point)),
        std.err = ifelse(n.risk == 0, NA, std.err)
      )
  }

  # difference of survival rate by time-point
  bylist <- if (pairwise & !is.null(km_fit$strata)) {
    t(combn(grps, 2))
  } else if (!pairwise & !is.null(km_fit$strata)) {
    t(combn(grps, 2))[which(t(combn(grps, 2))[, 1] == grps[1]), , drop = FALSE]
  } else {
    NULL
  }
  surv_rate_diff <- if (!is.null(time_point) & !is.null(bylist)) {
    split(bylist, 1:nrow(bylist)) %>%
      purrr::map(function(x) {
        filter(surv_tp_rate, group %in% x) %>%
          split(time_point) %>%
          purrr::map(function(x) {
            tibble::tibble(
              group = paste(rev(x$group), collapse = " - "),
              time = unique(x$time),
              surv.diff = diff(x$surv),
              std.err = sqrt(sum(x$std.err^2)),
              lower = surv.diff - qnorm(1 - 0.05 / 2) * std.err,
              upper = surv.diff + qnorm(1 - 0.05 / 2) * std.err,
              pval = if (is.na(std.err)) {
                NA
              } else {
                2 * (1 - stats::pnorm(abs(surv.diff) / std.err))
              }
            )
          }) %>%
          purrr::list_rbind()
      }) %>%
      purrr::list_rbind()
  }

  # survival range in overall time
  range_evt <- surv_overall_rate %>%
    filter(n.event != 0) %>%
    group_by(group) %>%
    summarise(
      event_min = min(time, na.rm = TRUE),
      event_max = max(time, na.rm = TRUE),
      .groups = "drop"
    )
  range_cnsr <- surv_overall_rate %>%
    filter(n.censor != 0) %>%
    group_by(group) %>%
    summarise(
      censor_min = min(time, na.rm = TRUE),
      censor_max = max(time, na.rm = TRUE),
      .groups = "drop"
    )
  range_tb <- full_join(range_evt, range_cnsr, by = c("group")) %>%
    rowwise() %>%
    mutate(
      min = min(event_min, censor_min, na.rm = TRUE),
      max = max(event_max, censor_max, na.rm = TRUE)
    )

  structure(
    list(
      surv = list(
        median = surv_tb,
        quantile = quant,
        overall = surv_overall_rate,
        time_point = surv_tp_rate,
        range = range_tb
      ),
      surv_diff = list(
        test = surv_test,
        rate = surv_rate_diff
      ),
      params = list(
        formula = format(c(formula, formula_strata)),
        quantile = quantile,
        time_point = time_point,
        conf_type = conf_type,
        conf_level = conf_level,
        strata = strata,
        pval_method = pval_method,
        pairwise = pairwise
      )
    ),
    class = "s_survival"
  )
}


# s_get_coxph ----

#' Summarize Cox Proportional Hazards Regression Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function summarizes `coxph` model with or without stratification, and provides
#' hazards ratio, confidence interval and p-value for common clinical survival analysis.
#'
#' @param data (`data.frame`)\cr a data frame as input.
#' @param formula (`formula`)\cr a formula with survival object.
#' @param ties (`string`)\cr string specifying the the method for tie handling,
#'  default is efron. See more details in [survival::coxph()].
#' @param conf_level (`numeric`)\cr significance level for a two-side confidence
#'  interval on survival curve, default is 0.95.
#' @param strata (`character`)\cr a character vector used for stratification.
#' @param pval_method (`string`)\cr string specifying the the method for tie handling,
#'  default is to present all three methods (Likelihood ratio test, Wald test and
#'  Score (logrank) test).
#' @param ... other arguments to be passed to [survival::coxph()].
#'
#' @return
#' An object of class `s_coxph` is a list contains hazards ratio and p-value tables.
#'
#' @export
#'
#' @examples
#' data("whas500")
#'
#' # reorder the grouping variable
#' dat <- whas500 %>%
#'   mutate(
#'     AFB = factor(AFB, levels = c(1, 0))
#'   )
#' s_get_coxph(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)
#'
#' # specify the stratified log-rank test
#' s_get_coxph(
#'   data = dat,
#'   formula = Surv(LENFOL, FSTAT) ~ AFB,
#'   strata = c("AGE", "GENDER")
#' )
#'
#' # dummy three groups
#' set.seed(123)
#' subj <- sample(dat$ID, 100)
#' dat2 <- whas500 %>%
#'   mutate(
#'     AFB = case_when(
#'       ID %in% subj ~ 2,
#'       TRUE ~ AFB
#'     ),
#'     AFB = factor(AFB, levels = c(1, 2, 0))
#'   )
#' s_get_coxph(data = dat2, formula = Surv(LENFOL, FSTAT) ~ AFB)
#'
s_get_coxph <- function(data,
                        formula,
                        ties = c("efron", "breslow", "exact"),
                        conf_level = 0.95,
                        strata = NULL,
                        pval_method = c("all", "log", "sc", "wald"),
                        ...) {
  assert_class(data, "data.frame")
  assert_formula(formula)
  assert_number(conf_level, lower = 0, upper = 1)
  assert_subset(strata, names(data))
  ties <- match.arg(ties, c("efron", "breslow", "exact"), several.ok = FALSE)
  pval_method <- match.arg(pval_method, c("all", "log", "sc", "wald"), several.ok = FALSE)

  formula <- if (!is.null(strata)) {
    as.formula(
      paste0(format(formula), " + strata(", paste(strata, collapse = ", "), ")")
    )
  } else {
    formula
  }

  cox_fit <- survival::coxph(formula, data = data, ties = ties, ...)
  cox_ss <- summary(cox_fit, conf.int = conf_level, extend = TRUE)

  pval_name <- switch(pval_method,
    all = c("logtest", "sctest", "waldtest"),
    log = "logtest",
    sc = "sctest",
    wald = "waldtest"
  )
  pval_tb <- cox_ss[pval_name] %>%
    dplyr::bind_rows(.id = "method")

  hr_tb <- tibble::tibble(
    variable = row.names(cox_ss$conf.int),
    n = cox_ss$n,
    event = cox_ss$nevent,
    hr = cox_ss$conf.int[, 1],
    lower = cox_ss$conf.int[, 3],
    upper = cox_ss$conf.int[, 4]
  )

  structure(
    list(
      hr = hr_tb,
      pval = pval_tb,
      params = list(
        formula = format(formula),
        ties = ties,
        conf_level = conf_level,
        strata = strata,
        pval_method = pval_method
      )
    ),
    class = "s_coxph"
  )
}
