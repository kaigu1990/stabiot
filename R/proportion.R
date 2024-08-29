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
#' @param by (`string`)\cr an optional variable to group by. If null, use the whole data.
#' @param by.level (`vector`)\cr an optional vector for encoding `var` as a factor
#'  and the first level will be as the reference group. If null, use the default
#'  order to encode.
#' @param resp (`numeric` or `character`)\cr an option to define which one as the
#'  respondor in the elements of `var`. By default, the positive and maximal one if
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
#' @references
#' SAS code for your reference with consistent results.
#'
#' - The rate and difference of proportions are estimated:
#' ```
#' proc means data=dta noprint;
#'   class trtp orr strata1 strata2 strata3;
#'   types trtp*orr*strata1*strata2*strata3;
#'   output out=mat(drop=_type_ rename=_freq_=count);
#' run;
#'
#' ods listing close;
#' proc freq data=mat;
#'   by trtp;
#'   weight count/zeros;
#'   tables orr /binomial(level="1") alpha=0.05;
#'   exact binomial;
#'   ods output binomial=orrci;
#' run;
#'
#' proc freq data=mat(where=(trtp in ('PBO' 'TRT1')));
#'   weight count/zeros;
#'   tables trtp*orr /riskdiff(CL=wald column=1) alpha=0.05;
#'   ods output PdiffCLs=diff;
#'   run;
#' ods listing;
#' ```
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
#'   orr = sample(c(1, 0), 150, TRUE),
#'   trtp = factor(rep(c("TRT1", "TRT2", "PBO"), each = 50)),
#'   strata1 = factor(sample(c("A", "B"), 150, TRUE)),
#'   strata2 = factor(sample(c("C", "D"), 150, TRUE)),
#'   strata3 = factor(sample(c("E", "F"), 150, TRUE))
#' )
#'
#' # not having by variable:
#' s_propci(dta, var = "orr")
#'
#' # two levels of by variable:
#' s_propci(dta, var = "orr", by = "trtp")
#' # opposite order of trtp with above:
#' s_propci(dta, var = "orr", by = "trtp", by.level = c("TRT1", "TRT2", "PBO"), resp = 1)
s_propci <- function(data,
                     var,
                     by = NULL,
                     by.level = NULL,
                     resp = NULL,
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
  assert_subset(resp, data[[var]])
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

  object <- h_prep_prop(data, var = var, by = by, by.level = by.level, resp = resp)
  by <- object$by

  est_res <- object$data %>%
    count(!!sym(by), !!sym(var)) %>%
    add_count(!!sym(by), wt = .data$n, name = "tot") %>%
    filter(!!sym(var) == object$resp) %>%
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
    grps <- object$by.level
    bylist <- t(combn(grps, 2))[which(t(combn(grps, 2))[, 1] == grps[1]), , drop = FALSE]
    split(bylist, 1:nrow(bylist)) %>%
      purrr::map(function(x) {
        res <- filter(object$data, !!sym(by) %in% x) %>%
          count(!!sym(by), !!sym(var)) %>%
          add_count(!!sym(by), wt = .data$n, name = "tot") %>%
          filter(!!sym(var) == object$resp) %>%
          tidyr::pivot_wider(names_from = by, values_from = c("n", "tot"))
        chisq <- stats::prop.test(
          x = as.numeric(res[1, grep("n_", colnames(res)), drop = TRUE]),
          n = as.numeric(res[1, grep("tot_", colnames(res)), drop = TRUE]),
          correct = FALSE
        )
        tibble::tibble(
          reference = x[1],
          comparison = x[2],
          tibble::as_tibble(
            DescTools::BinomDiffCI(
              x1 = res[[3]], n1 = res[[5]], x2 = res[[2]], n2 = res[[4]],
              method = diff.method, sides = alternative, conf.level = conf.level
            )
          ),
          pval = chisq$p.value
        )
      }) %>%
      purrr::list_rbind()
  }

  structure(
    list(
      data = object$data,
      prop_est = est_res,
      prop_diff = diff_res,
      params = list(
        var = var,
        by = by,
        by.level = object$by.level,
        resp = object$resp,
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
#' @references
#' - The common odds ratio and stratified odds ratio with CMH test and conditional logistic are estimated:
#' ```
#' ods listing close;
#'   proc freq data=mat(where=(trtp in ('PBO' 'TRT1')));
#'   weight count/zeros;
#'   tables trtp*orr / relrisk chisq;
#'   ods output ChiSq=pval RelativeRisks=ci;
#' run;
#'
#' proc freq data=mat(where=(trtp in ('PBO' 'TRT1')));
#'   weight count/zeros;
#'   tables strata1*strata2*strata3*trtp*orr / relrisk cmh;
#'   ods output cmh=cmhpval CommonRelRisks=cmhci;
#' run;
#'
#' proc logistic data=mat;
#'   freq count;
#'   class trtp strata1 strata2 strata3 / param=ref ref=first;
#'   strata strata1 strata2 strata3;
#'   model orr(event='1')=trtp;
#' run;
#' ods listing;
#' ```
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
#'   or.glm = TRUE,
#'   strata = c("strata1", "strata2", "strata3"),
#'   strata.method = "clogit"
#' )
s_odds_ratio <- function(data,
                         var,
                         by,
                         by.level = NULL,
                         resp = NULL,
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
  assert_subset(resp, data[[var]])
  assert_subset(strata, names(data))
  assert_number(conf.level, lower = 0, upper = 1)
  assert_logical(or.glm)
  assert_logical(correct)
  assert_logical(exact)
  or.method <- match.arg(or.method, c("wald", "mle", "midp"), several.ok = FALSE)
  strata.method <- match.arg(strata.method, c("CMH", "clogit"), several.ok = FALSE)

  object <- h_prep_prop(data, var = var, by = by, by.level = by.level, resp = resp)

  mat <- object$data %>%
    count(!!sym(by), !!sym(var)) %>%
    arrange(!!sym(var) == object$resp, !!sym(by)) %>%
    tidyr::pivot_wider(names_from = var, values_from = "n") %>%
    tibble::column_to_rownames(var = by) %>%
    as.matrix()

  grps <- object$by.level
  bylist <- t(combn(grps, 2))[which(t(combn(grps, 2))[, 1] == grps[1]), , drop = FALSE]

  or_res <- if (or.glm) {
    mod <- stats::glm(
      as.formula(paste(var, "~", by)),
      data = object$data,
      family = stats::binomial(link = "logit")
    )
    data.frame(
      exp(c(coef(mod))),
      exp(confint(mod, level = conf.level)),
      coef(summary(mod))[, "Pr(>|z|)"],
      stringsAsFactors = F
    ) %>%
      .[-1, ] %>%
      mutate(group = sub(by, "", row.names(.)))
  } else {
    split(bylist, 1:nrow(bylist)) %>%
      purrr::map(function(x) {
        data.frame(
          t(DescTools::OddsRatio(mat[x, ], method = or.method, conf.level = conf.level)),
          pval = stats::fisher.test(mat[x, ], conf.level = conf.level)$p.value,
          stringsAsFactors = FALSE
        ) %>%
          mutate(group = x[2])
      }) %>%
      purrr::list_rbind()
  }
  or_res <- tibble::tibble(
    !!!setNames(or_res[, c(5, 1:4)], c("group", "or.est", "lwr.ci", "upr.ci", "pval"))
  )

  stra_or_res <- if (!is.null(strata)) {
    if (strata.method == "CMH") {
      split(bylist, 1:nrow(bylist)) %>%
        purrr::map(function(x) {
          df <- filter(object$data, !!sym(by) %in% x) %>%
            mutate(
              !!sym(by) := droplevels(!!sym(by))
            )
          tab <- stats::xtabs(
            as.formula(paste("~", paste(c(by, var, strata), collapse = "+"))),
            data = df
          )
          # get the number of factors for each stratification
          grpn <- strata %>%
            purrr::map(~ count(df, df[[.x]])) %>%
            purrr::map_int(nrow)
          tb <- as.table(array(c(tab), dim = c(2, 2, prod(grpn))))
          mod <- stats::mantelhaen.test(
            tb,
            conf.level = conf.level,
            correct = correct, exact = exact
          )
          tibble::tibble(
            group = x[2],
            or.est = as.numeric(mod$estimate),
            lwr.ci = mod$conf.int[1],
            upr.ci = mod$conf.int[2],
            pval = mod$p.value
          )
        }) %>%
        purrr::list_rbind()
    } else {
      mod <- survival::clogit(
        formula = as.formula(paste0(
          var, " ~ ", by, " + strata(",
          paste(strata, collapse = ", "), ")"
        )),
        data = data
      )

      # default the first level is regarded as the reference
      tibble::tibble(
        group = gsub(pattern = paste0("^", by), x = names(coef(mod)), ""),
        or.est = as.numeric(exp(coef(mod))),
        lwr.ci = exp(confint(mod, level = conf.level)[, 1]),
        upr.ci = exp(confint(mod, level = conf.level)[, 2]),
        pval = coef(summary(mod))[, "Pr(>|z|)"]
      )
    }
  } else {
    NULL
  }

  structure(
    list(
      data = object$data,
      or = or_res,
      strata_or = stra_or_res,
      params = list(
        var = var,
        by = by,
        by.level = object$by.level,
        resp = object$resp,
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
