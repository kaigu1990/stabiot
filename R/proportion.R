# s_propci ----

#' Computing Proportion and Odds Ratio
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Compute confidence interval for proportion and difference of binomial response,
#' and odds ratio using the `DescTools` package. As an alternative, use `stats::glm`
#' with `logit` link to estimate the odds ratio. Regarding to stratified odds ratio,
#' use Cochran-Mantel-Haenszel test (`stats::mantelhaen.test`) or conditional logistic
#' regression (`survival::clogit`) to handle with. However this CMH test function is
#' limited to the 2 by 2 by K scenario, whereas conditional logistic regression
#' can handle 2 by N by K case.
#'
#' @note
#' * The proportion difference can only support the two-levels for `by` variable.
#' * Compute odds ratio when `or.glm` is FALSE, 0.5 will be added to 2 x 2 table
#' in case of zero entries. More details can be found in [DescTools::OddsRatio].
#' * The arguments of this function can only match few parameters of SAS `proc freq`
#' to calculate the odds ratio. For example, `mantelhaen.test` corresponds the
#' Mantel-Haenszel Estimator with the same reference article, and using `correct = FALSE`
#' can produce the Wald confidence interval same as the default parameters of SAS.
#'
#' @name prop_odds_ratio
#' @order 1
NULL


#' @describeIn prop_odds_ratio Computes the confidence interval of proportion/response
#' rate and difference of two proportions/rates.
#'
#' @param data (`data.frame`)\cr a data frame as input.
#' @param var (`string`)\cr target variable name for estimation.
#' @param by (`string`)\cr a optional variable to group by. If null, use the whole data.
#' @param by.level (`vector`)\cr an optional vector for encoding `var` as a factor
#'  and the second level will be as the reference group. If null, use the default
#'  order to encode.
#' @param event (`numeric` or `character`)\cr an option to define which one as the
#'  event in the elements of `var`. By default, the positive and maximal one if
#'  the `var` variable is numeric, or the first one of if the `var` variable is
#'  character/factor.
#' @param conf.level (`numeric`)\cr significance level for the returned confidence
#'  interval.
#' @param method (`string`)\cr string specifying which method to calculate the
#'  confidence interval for binomial proportions, default is Clopper-Pearson. More
#'  details see [DescTools::BinomCI].
#' @param diff.method (`string`)\cr string specifying which method to calculate the
#'  confidence interval for difference between binomial proportions, default is
#'  Wald. More details see [DescTools::BinomDiffCI].
#' @param alternative (`string`)\cr string specifying the alternative hypothesis,
#'    must be one of "two.sided" (default), "greater" or "less".
#' @param ... other arguments to be passed to [DescTools::BinomCI].
#'
#'
#' @return
#' * `s_propci` returns an object of class `prop_ci` that is a list contains
#' proportion rate, proportion difference and input arguments.
#'
#' @export
#'
#' @examples
#' set.seed(12)
#' dta <- data.frame(
#'   orr = sample(c(1, 0), 100, TRUE),
#'   trtp = factor(rep(c("TRT", "PBO"), each = 50)),
#'   strata1 = factor(sample(c("A", "B"), 100, TRUE)),
#'   strata2 = factor(sample(c("C", "D"), 100, TRUE)),
#'   strata3 = factor(sample(c("E", "F"), 100, TRUE))
#' )
#'
#' # not having by variable:
#' s_propci(dta, var = "orr")
#'
#' # two levels of by variable:
#' s_propci(dta, var = "orr", by = "trtp")
#' # opposite order of trtp with above:
#' s_propci(dta, var = "orr", by = "trtp", by.level = c("TRT", "PBO"), event = 1)
s_propci <- function(data,
                     var,
                     by = NULL,
                     by.level = NULL,
                     event = NULL,
                     conf.level = 0.95,
                     method = c(
                       "clopper-pearson", "wald", "waldcc", "wilson",
                       "wilsoncc", "modified wilson", "jeffreys",
                       "modified jeffreys", "agresti-coull"
                     ),
                     diff.method = c(
                       "wald", "waldcc", "score", "scorecc", "mn", "mee",
                       "blj", "ha", "hal", "jp"
                     ),
                     alternative = c("two.sided", "less", "greater"),
                     ...) {
  assert_class(data, "data.frame")
  assert_subset(var, names(data), empty.ok = FALSE)
  assert_subset(by, names(data))
  assert_subset(event, data[[var]])
  assert_number(conf.level, lower = 0, upper = 1)
  method <- match.arg(method, c(
    "clopper-pearson", "wald", "waldcc", "wilson", "wilsoncc",
    "modified wilson", "jeffreys", "modified jeffreys", "agresti-coull"
  ), several.ok = FALSE)
  diff.method <- match.arg(diff.method, c(
    "wald", "waldcc", "score", "scorecc", "mn", "mee",
    "blj", "ha", "hal", "jp"
  ), several.ok = FALSE)
  alternative <- match.arg(alternative, c(
    "two.sided", "less", "greater"
  ), several.ok = FALSE)

  object <- h_prep_prop(data, var = var, by = by, by.level = by.level, event = event)
  by <- object$by

  est_res <- object$data %>%
    count(!!sym(by), !!sym(var)) %>%
    add_count(!!sym(by), wt = .data$n, name = "tot") %>%
    filter(!!sym(var) == object$event) %>%
    split(as.formula(paste("~", by))) %>%
    purrr::map(function(x) {
      tibble::tibble(
        group = x[[by]],
        tibble::as_tibble(
          DescTools::BinomCI(
            x = x$n, n = x$tot, method = method, conf.level = conf.level,
            sides = alternative, ...
          )
        )
      )
    }) %>%
    purrr::list_rbind()

  diff_res <- if (!is.null(object$by.level)) {
    object$data %>%
      count(!!sym(by), !!sym(var)) %>%
      add_count(!!sym(by), wt = .data$n, name = "tot") %>%
      filter(!!sym(var) == object$event) %>%
      tidyr::pivot_wider(names_from = by, values_from = c("n", "tot")) %>%
      split(as.formula(paste("~", var))) %>%
      purrr::map(function(x) {
        tibble::tibble(
          group = paste0(object$by.level, collapse = " - "),
          tibble::as_tibble(
            DescTools::BinomDiffCI(
              x1 = x[[2]], n1 = x[[4]], x2 = x[[3]], n2 = x[[5]],
              method = diff.method, sides = alternative, conf.level = conf.level
            )
          )
        )
      }) %>%
      purrr::list_rbind()
  }

  structure(
    list(
      prop_est = est_res,
      prop_diff = diff_res,
      params = list(
        var = var,
        by = by,
        by.level = object$by.level,
        event = object$event,
        conf.level = conf.level,
        method = method,
        diff.method = diff.method,
        alternative = alternative
      )
    ),
    class = "prop_ci"
  )
}


# s_odds_ratio ----

#' @describeIn prop_odds_ratio Computes the confidence interval of common odds ratio.
#' And also provides stratified odds ratio with Cochran-Mantel-Haenszel chi-squared
#' test and conditional logistic regression.
#'
#' @param strata (`vector`)\cr a character vector is used for stratification.
#' @param or.glm (`logical`)\cr a logical indicating whether to use logit method to
#'  calculate the odds ratio. If TRUE, the `glm` with logit is used, otherwise the
#'  common method from `DescTools::OddsRatio`.
#' @param or.method (`string`)\cr string indicating the method from `DescTools::OddsRatio`
#'  used to calculate odds ratio when the `or.glm` is FALSE.
#' @param strata.method (`string`)\cr string indicating the method used to calculate
#'  stratified odds ratio, Cochran-Mantel-Haenszel Chi-Squared test (CMH) or conditional
#'  logistic regression (clogit).
#' @param correct (`logical`)\cr a logical indicating whether to apply continuity
#'  correction when computing the test statistic.
#' @param exact (`logical`)\cr a logical indicating whether the Mantel-Haenszel
#'  test or the exact conditional test (given the strata margins) should be computed.
#'
#' @return
#' * `s_odds_ratio` returns an object of class `or_ci` that is a list contains
#' odds ratio with or without stratification and input arguments.
#'
#' @export
#'
#' @examples
#'
#' # without stratification:
#' s_odds_ratio(dta, var = "orr", by = "trtp", or.method = "wald")
#'
#' # with three stratifications, strata1 - strata3:
#' s_odds_ratio(
#'   dta,
#'   var = "orr", by = "trtp",
#'   strata = c("strata1", "strata2", "strata3"),
#'   strata.method = "CMH",
#'   correct = FALSE
#' )
#'
#' # using condition logistic regression:
#' s_odds_ratio(
#'   dta,
#'   var = "orr", by = "trtp",
#'   strata = c("strata1", "strata2", "strata3"),
#'   strata.method = "clogit"
#' )
s_odds_ratio <- function(data,
                         var,
                         by,
                         by.level = NULL,
                         event = NULL,
                         strata = NULL,
                         conf.level = 0.95,
                         or.glm = FALSE,
                         or.method = c("wald", "mle", "midp"),
                         strata.method = c("CMH", "clogit"),
                         correct = FALSE,
                         exact = FALSE) {
  assert_class(data, "data.frame")
  assert_subset(var, names(data), empty.ok = FALSE)
  assert_subset(by, names(data), empty.ok = FALSE)
  assert_subset(event, data[[var]])
  assert_subset(strata, names(data))
  assert_number(conf.level, lower = 0, upper = 1)
  assert_logical(or.glm)
  assert_logical(correct)
  assert_logical(exact)
  or.method <- match.arg(or.method, c("wald", "mle", "midp"), several.ok = FALSE)
  strata.method <- match.arg(strata.method, c("CMH", "clogit"), several.ok = FALSE)

  object <- h_prep_prop(data, var = var, by = by, by.level = by.level, event = event)
  if (length(object$by.level) != 2) {
    stop("The by.level should have two levels as input.")
  }

  or_res <- if (or.glm) {
    mod <- stats::glm(
      as.formula(paste(var, "~", by)),
      data = object$data,
      family = stats::binomial(link = "logit")
    )
    exp(c(coef(mod)[-1], confint(mod, level = conf.level)[-1, ]))
  } else {
    object$data %>%
      count(!!sym(by), !!sym(var)) %>%
      arrange(!!sym(var) == object$event, !!sym(by)) %>%
      tidyr::pivot_wider(names_from = var, values_from = "n") %>%
      tibble::column_to_rownames(var = by) %>%
      as.matrix() %>%
      # follow the preferred 2x2 table structure
      DescTools::OddsRatio(method = or.method, conf.level = conf.level)
  }
  or_res <- tibble::tibble(
    !!!setNames(or_res, c("or.est", "lwr.ci", "upr.ci"))
  )

  stra_or_res <- if (!is.null(strata)) {
    if (strata.method == "CMH") {
      assert_set_equal(length(unique(data[[var]])), 2)
      tab <- stats::xtabs(
        as.formula(paste("~", paste(c(by, var, strata), collapse = "+"))),
        data = data
      )
      # get the number of factors for each stratification
      grpn <- strata %>%
        purrr::map(~ count(data, data[[.x]])) %>%
        purrr::map_int(nrow)
      tb <- as.table(array(c(tab), dim = c(2, 2, prod(grpn))))
      mod <- stats::mantelhaen.test(
        tb,
        conf.level = conf.level,
        correct = correct, exact = exact
      )
      tibble::tibble(
        !!!setNames(
          c(mod$estimate, mod$conf.int, mod$p.value),
          c("or.est", "lwr.ci", "upr.ci", "pval")
        )
      )
    } else {
      mod <- survival::clogit(
        formula = as.formula(paste0(
          var, " ~ ", by, " + strata(",
          paste(strata, collapse = ", "), ")"
        )),
        data = data
      )

      # defaultly the first level is regarded as the reference
      names(coef(mod)) %>%
        purrr::map(
          ~ data.frame(
            or.est = exp(coef(mod)[.x]),
            lwr.ci = exp(confint(mod, level = conf.level)[.x, 1]),
            upr.ci = exp(confint(mod, level = conf.level)[.x, 2]),
            row.names = gsub(pattern = paste0("^", by), x = .x, "")
          )
        ) %>%
        purrr::list_rbind() %>%
        tibble::tibble()
    }
  } else {
    NULL
  }

  structure(
    list(
      or = or_res,
      strata_or = stra_or_res,
      params = list(
        var = var,
        by = by,
        by.level = object$by.level,
        event = object$event,
        strata = strata,
        conf.level = conf.level,
        or.method = ifelse(or.glm, "logit", or.method),
        strata.method = ifelse(
          !is.null(strata),
          strata.method,
          NA
        )
      )
    ),
    class = "or_ci"
  )
}


# h_prep_prop ----

#' @describeIn prop_odds_ratio Helper Function for Pre-processing Proportion Data.
#'
#' @export
#'
h_prep_prop <- function(data,
                        var,
                        by,
                        by.level,
                        event) {
  if (is.null(by)) {
    by <- "Total"
    data[[by]] <- "Total"
  } else {
    if (!is.null(by.level)) {
      assert_set_equal(by.level, as.character(unique(data[[by]])))
      data[[by]] <- factor(data[[by]], levels = by.level)
    } else {
      if (is.factor(data[[by]])) {
        by.level <- levels(data[[by]])
      } else {
        data[[by]] <- factor(data[[by]], levels = unique(data[[by]]))
        by.level <- levels(data[[by]])
      }
    }
  }

  if (is.null(event)) {
    event <- if (is.numeric(data[[var]])) {
      max(unique(data[[var]])[unique(data[[var]]) > 0])
    } else if (is.character(event)) {
      unique(data[[var]])[1]
    } else if (is.factor(data[[var]])) {
      levels(data[[var]])[1]
    }
  }

  list(
    data = data,
    by = by,
    by.level = by.level,
    event = event
  )
}
