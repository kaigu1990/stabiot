# s_propci ----

#' Computing Proportion and Odds Ratio
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Compute confidence interval for proportion and difference of binomial response,
#' and odds ratio using the `DescTools` package. As an alternative, use `stats::glm`
#' with `logit` link to estimate the odds ratio. Regarding to stratified odds ratio,
#' use Cochran-Mantel-Haenszel test (`stats::mantelhaen.test`) or conditional logistic
#' regression (`survival::clogit`) to handle with.
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
#' @note
#' The proportion difference can only support the two-levels for `by` variable.
#'
#' @return
#' * `s_propci` returns an object of class `prop_ci` that is a list contains
#' proportion rate, proportion difference and arguments.
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
#' # Not having by variable:
#' s_propci(dta, var = "orr")
#'
#' # Two levels of by variable:
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
    dplyr::filter(!!sym(var) == object$event) %>%
    split(as.formula(paste("~", by))) %>%
    purrr::map(function(x) {
      tibble::tibble(
        group = x[[by]],
        tibble::as_tibble(
          DescTools::BinomCI(
            x = x$n, n = x$tot, method = method,
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
      dplyr::filter(!!sym(var) == object$event) %>%
      tidyr::pivot_wider(names_from = by, values_from = c(n, tot)) %>%
      split(as.formula(paste("~", var))) %>%
      purrr::map(function(x) {
        tibble::tibble(
          group = paste0(object$by.level, collapse = " - "),
          tibble::as_tibble(
            DescTools::BinomDiffCI(
              x1 = x[[2]], n1 = x[[4]], x2 = x[[3]], n2 = x[[5]],
              method = diff.method, sides = alternative
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
      data[[by]] <- factor(data[[by]], levels = unique(data[[by]]))
      by.level <- unique(data[[by]])
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
