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
#' @param rho (`number`)\cr a scalar parameter that controls the method
#'  of comparing survival curves. Default is 0 that is log-rank , others options
#'  can see [survival::survdiff()].
#' @param pairwise (`logical`)\cr whether to conduct the pairwise comparison.
#' @param survdiff (`logical`)\cr whether to test survival curve differences.
#' @param ... other arguments to be passed to [survival::survfit()].
#'
#' @order 1
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
#' estimates, and `survival::survdiff()` to compute the p-value of comparing
#' survival curves. The SE and p-value of difference rate are simply computed
#' using normal distribution and Z statistic.
#'
#' Stratification can only alter the testing survival curves, therefore there will
#' be no changes to other outputs when the `strata` argument is defined.
#'
#' @references
#' SAS code for your reference with consistent results.
#' ```
#' ods output Quartiles=km_quartiles SurvivalPlot=km_survplot SurvDiff=km_survdiff;
#' ods listing close;
#' proc lifetest data=whas500 method=KM alphaqt=0.05
#'   timelist=12 36 60 reduceout stderr
#'   conftype=loglog outsurv=surv_est;
#'   time LENFOL*FSTAT(0);
#'   strata AFB / test=logrank diff=control('1');
#'   /* 	strata AGE GENDER / group=AFB; */
#' run;
#' ods listing;
#' ```
#'
#' @export
#'
#' @examples
#' data("whas500")
#'
#' # reorder the grouping variable
#' dat <- whas500 |>
#'   dplyr::mutate(
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
#' dat2 <- whas500 |>
#'   dplyr::mutate(
#'     AFB = dplyr::case_when(
#'       ID %in% subj ~ 2,
#'       TRUE ~ AFB
#'     ),
#'     AFB = factor(AFB, levels = c(1, 2, 0))
#'   )
#'
#' s_get_survfit(
#'   data = dat2,
#'   formula = Surv(LENFOL, FSTAT) ~ AFB,
#'   time_point = c(12, 36, 60),
#'   pairwise = FALSE
#' )
s_get_survfit <- function(data,
                          formula,
                          quantile = c(0.25, 0.75),
                          time_point = NULL,
                          conf_type = c("log-log", "log", "plain"),
                          conf_level = 0.95,
                          strata = NULL,
                          rho = 0,
                          pairwise = FALSE,
                          survdiff = TRUE,
                          ...) {
  assert_class(data, "data.frame")
  assert_formula(formula)
  assert_numeric(quantile, lower = 0, upper = 1)
  assert_numeric(time_point, null.ok = TRUE)
  assert_number(conf_level, lower = 0, upper = 1)
  assert_subset(strata, names(data))
  assert_int(rho, lower = 0, upper = 1)
  assert_logical(pairwise)
  conf_type <- match.arg(conf_type, c("log-log", "log", "plain"), several.ok = FALSE)

  formula <- as.formula(formula)
  km_fit <- survival::survfit(
    data = data,
    formula = formula,
    conf.int = conf_level,
    conf.type = conf_type,
    ...
  )

  grps <- if (is.null(km_fit$strata)) {
    grp_var <- "new_col"
    data[[grp_var]] <- "Total"
  } else {
    grp_var <- attr(stats::terms(formula), "term.labels")
    sub(paste0(grp_var, "="), "", names(km_fit$strata))
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
    tidyr::pivot_longer(tidyr::everything(), names_to = "group") %>%
    mutate(
      group = sub(paste0(grp_var, "="), "", .data$group),
      type = rep(c("time", "lower", "upper"), each = length(quantile) * length(grps)),
      quantile = rep(quantile * 100, each = length(grps), times = 3)
    ) %>%
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  # median survival time
  surv_tb <- if (is.null(km_fit$strata)) {
    broom::glance(km_fit) %>%
      mutate(group = grps) %>%
      select("group", n = "nobs", "events", "median", lower = "conf.low", upper = "conf.high")
  } else {
    tibble::as_tibble(summary(km_fit)$table) %>%
      mutate(group = grps) %>%
      select("group", n = "n.start", "events", "median", lower = "0.95LCL", upper = "0.95UCL")
  }

  # overall survival rate
  cols <- c("time", "n.risk", "n.event", "n.censor", "surv", "std.err", "lower", "upper")
  overall <- summary(km_fit, censored = TRUE)
  surv_overall_rate <- if (is.null(km_fit$strata)) {
    tibble::as_tibble(overall[cols]) %>%
      mutate(group = grps)
  } else {
    evt_ind <- which(overall$time < dplyr::lag(overall$time)) - 1
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
        std.err = ifelse(.data$n.risk == 0, NA, .data$std.err)
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
        filter(surv_tp_rate, .data$group %in% x) %>%
          split(time_point) %>%
          purrr::map(function(x) {
            tibble::tibble(
              reference = x[["group"]][1],
              comparison = x[["group"]][2],
              time = unique(x$time),
              surv.diff = diff(x$surv),
              std.err = sqrt(sum(x$std.err^2)),
              lower = .data$surv.diff - stats::qnorm(1 - 0.05 / 2) * .data$std.err,
              upper = .data$surv.diff + stats::qnorm(1 - 0.05 / 2) * .data$std.err,
              pval = if (is.na(.data$std.err)) {
                NA
              } else {
                2 * (1 - stats::pnorm(abs(.data$surv.diff) / .data$std.err))
              }
            )
          }) %>%
          purrr::list_rbind()
      }) %>%
      purrr::list_rbind()
  }

  # test survival curves
  surv_test <- if (!is.null(km_fit$strata) & survdiff) {
    survdiff <- h_pairwise_survdiff(
      formula = formula,
      strata = strata,
      data = data,
      rho = rho
    )
    tibble::tibble(
      reference = bylist[, 1],
      comparison = bylist[, 2],
      method = survdiff$method,
      pval = as.numeric(mapply(function(x, y) {
        survdiff$p.value[x, y]
      }, bylist[, 2], bylist[, 1]))
    )
  }

  # survival range in overall time
  range_evt <- surv_overall_rate %>%
    filter(.data$n.event != 0) %>%
    group_by(.data$group) %>%
    summarise(
      event_min = min(.data$time, na.rm = TRUE),
      event_max = max(.data$time, na.rm = TRUE),
      .groups = "drop"
    )
  range_cnsr <- surv_overall_rate %>%
    filter(.data$n.censor != 0) %>%
    group_by(.data$group) %>%
    summarise(
      censor_min = min(.data$time, na.rm = TRUE),
      censor_max = max(.data$time, na.rm = TRUE),
      .groups = "drop"
    )
  range_tb <- full_join(range_evt, range_cnsr, by = c("group")) %>%
    rowwise() %>%
    mutate(
      min = min(.data$event_min, .data$censor_min, na.rm = TRUE),
      max = max(.data$event_max, .data$censor_max, na.rm = TRUE)
    ) %>%
    ungroup()

  structure(
    list(
      data = data,
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
        formula = format(formula),
        var = grp_var,
        quantile = quantile,
        time_point = time_point,
        conf_type = conf_type,
        conf_level = conf_level,
        strata = strata,
        rho = rho,
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
#' @param pairwise (`logical`)\cr whether to conduct the pairwise comparison.
#' @param ... other arguments to be passed to [survival::coxph()].
#'
#' @order 1
#'
#' @return
#' An object of class `s_coxph` is a list contains hazards ratio and p-value tables.
#'
#' @references
#' SAS code for your reference with consistent results.
#' ```
#' ods output hazardratios=hrs;
#' ods output ParameterEstimates=hr_est;
#' ods listing close;
#' proc phreg data=whas500 alpha=0.05;
#'   class AFB (param=ref ref='1') AGE GENDER;
#'   model LENFOL*FSTAT(0)=AFB / ties=efron risklimits=wald;
#'   /* 	strata AGE GENDER; */
#'   hazardratio AFB / diff=ref;
#' quit;
#' ods listing;
#' ```
#'
#' @export
#'
#' @examples
#' data("whas500")
#'
#' # reorder the grouping variable
#' dat <- whas500 |>
#'   dplyr::mutate(
#'     AFB = factor(AFB, levels = c(1, 0))
#'   )
#' s_get_coxph(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)
#'
#' s_get_coxph(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB, pval_method = "sc")
#'
#' # specify the stratified analysis
#' s_get_coxph(
#'   data = dat,
#'   formula = Surv(LENFOL, FSTAT) ~ AFB,
#'   strata = c("AGE", "GENDER")
#' )
#'
#' # dummy three groups
#' set.seed(123)
#' subj <- sample(dat$ID, 100)
#' dat2 <- whas500 |>
#'   dplyr::mutate(
#'     AFB = dplyr::case_when(
#'       ID %in% subj ~ 2,
#'       TRUE ~ AFB
#'     ),
#'     AFB = factor(AFB, levels = c(1, 2, 0))
#'   )
#' s_get_coxph(data = dat2, formula = Surv(LENFOL, FSTAT) ~ AFB)
s_get_coxph <- function(data,
                        formula,
                        ties = c("efron", "breslow", "exact"),
                        conf_level = 0.95,
                        strata = NULL,
                        pval_method = c("all", "log", "sc", "wald"),
                        pairwise = FALSE,
                        ...) {
  assert_class(data, "data.frame")
  assert_formula(formula)
  assert_number(conf_level, lower = 0, upper = 1)
  assert_subset(strata, names(data))
  ties <- match.arg(ties, c("efron", "breslow", "exact"), several.ok = FALSE)
  pval_method <- match.arg(pval_method, c("all", "log", "sc", "wald"), several.ok = FALSE)

  pval_name <- switch(pval_method,
    all = c("logtest", "sctest", "waldtest"),
    log = "logtest",
    sc = "sctest",
    wald = "waldtest"
  )

  grp_var <- attr(stats::terms(formula), "term.labels")
  formula <- if (!is.null(strata)) {
    as.formula(
      paste0(format(formula), " + strata(", paste(strata, collapse = ", "), ")")
    )
  } else {
    as.formula(formula)
  }

  vardf <- unlist(data[, grp_var])
  group <- if (!is.factor(vardf)) {
    droplevels(as.factor(vardf))
  } else {
    droplevels(vardf)
  }
  grps <- levels(group)
  bylist <- if (pairwise) {
    t(combn(grps, 2))
  } else {
    t(combn(grps, 2))[which(t(combn(grps, 2))[, 1] == grps[1]), , drop = FALSE]
  }

  mods <- split(bylist, 1:nrow(bylist)) %>%
    purrr::map(function(x) {
      cox_ss <- filter(data, !!sym(grp_var) %in% x) %>%
        mutate(!!sym(grp_var) := droplevels(!!sym(grp_var))) %>%
        coxph(formula = formula, ties = ties, ...) %>%
        summary(conf.int = conf_level, extend = TRUE)
    })

  pval_tb <- mods %>%
    purrr::imap(function(x, idx) {
      x[pval_name] %>%
        dplyr::bind_rows(.id = "method") %>%
        mutate(
          reference = bylist[as.numeric(idx), ][1],
          comparison = bylist[as.numeric(idx), ][2]
        ) %>%
        select("reference", "comparison", "method", "test", "df", "pval" = "pvalue")
    }) %>%
    purrr::list_rbind()

  hr_tb <- mods %>%
    purrr::imap(function(x, idx) {
      tibble::tibble(
        reference = bylist[as.numeric(idx), ][1],
        comparison = bylist[as.numeric(idx), ][2],
        n = x[["n"]],
        events = x[["nevent"]],
        hr = x[["conf.int"]][, 1],
        lower = x[["conf.int"]][, 3],
        upper = x[["conf.int"]][, 4]
      )
    }) %>%
    purrr::list_rbind()

  structure(
    list(
      data = data,
      hr = hr_tb,
      pval = pval_tb,
      params = list(
        formula = format(formula),
        var = grp_var,
        group = grps,
        ties = ties,
        conf_level = conf_level,
        strata = strata,
        pval_method = pval_method
      )
    ),
    class = "s_coxph"
  )
}
