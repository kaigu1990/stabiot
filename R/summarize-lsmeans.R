# s_get_lsmeans ----

# s_get_lsmeans <- function(object, ...) {
#   UseMethod("s_get_lsmeans")
# }

#' Summarize Least-squares Means from Models
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' To estimate the Least-squares Means along with corresponding statistics and
#' confidence interval, and perform the hypothesis testing, using the `emmeans`
#' package.
#'
#' @param object (`fitted model`)\cr any fitted model that can be accepted by
#'  `emmeans::emmeans()`, such as object from `lm` for ANCOVA and `mmrm` for MMRM.
#' @param var (`string`)\cr string specifying the name of the predictor, such as
#'  it would be treatment group variable (TRT01PN) in the clinical trials. Default
#'  the first level is defined as reference group.
#' @param by (`string`)\cr string specifying the name of the grouped variable.
#'  The estimates and contrasts will be evaluated separately for each elements
#'  from the grouped variable.
#' @param contrast (`logical`)\cr whether to perform the contrasts, pairwise
#'  comparisons (required usage) and hypothesis testing or not, default is TRUE.
#' @param null (`numeric`)\cr null hypothesis value, it will also be referred to
#'  the margin of superiority and non-inferiority design, default is 0.
#' @param conf.level (`numeric`)\cr significance level for the returned confidence
#'  interval and hypothesis testing, default is 0.95.
#' @param alternative (`string`)\cr string specifying the alternative hypothesis,
#'  must be one of "two.sided" (default), "greater" or "less". See the special
#'  section below for more details.
#' @param ... other arguments to be passed to [emmeans::emmeans()].
#'
#' @rdname s_get_lsmeans
#' @order 1
#'
#' @return
#' An object of class `s_lsmeans` is a list contains several summary tables and
#' fit model as following components:
#'
#' - `model` information of fitted model.
#' - `lsm_est` estimate `tibble` table of Ls-means with corresponding statistics.
#' If `by` parameter is specified, all estimates for each `var` and `by` will
#' be presented.
#' - `lsm_contr` contrast `tibble` table with pairwise comparisons that contains
#' corresponding statistics, if the `contrast` is `TRUE`.
#' - `params` essential parameters.
#'
#' @note There is no any p value adjusted method involved.
#'
#' @details
#' For example, when the null hypothesis is θ equal to `0`, the `side` should be
#' `two.sided`, and `null` is defined as `0`. For the non-inferiority trial,
#' such as when the null hypothesis is θ inferiority to `-2`, the `side` should
#' be `greater`, and `null` is defined as `-2`. For the superiority trial,
#' when the null hypothesis is θ not superiority to `2`, the `side` should be
#' `greater`, and `null` is defined as `2`.
#'
#' @references
#' SAS code for your reference with consistent results.
#' ```
#' proc mixed data=fev_data;
#'   class ARMCD(ref='PBO') AVISIT RACE SEX USUBJID;
#'   model FEV1 = RACE SEX ARMCD ARMCD*AVISIT / ddfm=KR;
#'   repeated AVISIT / subject=USUBJID type=UN r rcorr;
#'   lsmeans ARMCD*AVISIT / cl alpha=0.05 diff slice=AVISIT;
#'   lsmeans ARMCD / cl alpha=0.05 diff;
#'   lsmestimate ARMCD*AVISIT [1,1 4] [-1,2 4] / cl upper alpha=0.025 testvalue=2;
#'   ods output lsmeans=lsm diffs=diff LSMEstimates=est;
#' run;
#' ```
#'
#' @export
#'
#' @examples
#' # refactor the level order:
#' data(fev_data, package = "mmrm")
#'
#' # fit MMRM model:
#' fit <- mmrm::mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   reml = TRUE, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear",
#'   data = fev_data
#' )
#'
#' # estimate ARMCD by visits:
#' s_get_lsmeans(fit, "ARMCD", by = "AVISIT")
#'
#' # estimate ARMCD by visits for superiority testing with null hypothesis of 2:
#' s_get_lsmeans(fit, "ARMCD", by = "AVISIT", null = 2, alternative = "greater")
#'
#' # fit ANCOVA model:
#' fit2 <- fev_data |>
#'   dplyr::filter(VISITN == 4 & !is.na(FEV1)) |>
#'   lm(formula = FEV1 ~ FEV1_BL + RACE + SEX + ARMCD)
#'
#' s_get_lsmeans(fit2, "ARMCD")
s_get_lsmeans <- function(object,
                          var,
                          by = NULL,
                          contrast = TRUE,
                          null = 0,
                          conf.level = 0.95,
                          alternative = c("two.sided", "less", "greater"),
                          ...) {
  assert_string(var)
  assert_string(by, null.ok = TRUE)
  assert_logical(contrast)
  assert_number(null)
  assert_number(conf.level, lower = 0, upper = 1)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"), several.ok = FALSE)

  ems <- if (is.null(by)) {
    emmeans::emmeans(object, var, ...)
  } else {
    emmeans::emmeans(object, var, by = by, ...)
  }

  lsm_ci <- tibble::as_tibble(
    data.frame(confint(ems, level = conf.level))
  )
  lsm_pval <- tibble::as_tibble(
    data.frame(emmeans::test(ems))
  )
  lsm_est_res <- suppressMessages(dplyr::full_join(lsm_ci, lsm_pval)) %>%
    dplyr::rename(
      "estimate" = "emmean"
    )

  contr <- if (contrast) {
    emmeans::contrast(ems, adjust = "none", method = "revpairwise")
  }
  contr_ci <- tibble::as_tibble(
    data.frame(confint(contr, level = conf.level))
  )
  side <- switch(alternative,
    two.sided = 0,
    less = -1,
    greater = 1
  )
  contr_pval <- tibble::as_tibble(
    data.frame(emmeans::test(contr, null = null, delta = 0, side = side, level = conf.level))
  )
  contr_est_res <- suppressMessages(dplyr::full_join(contr_ci, contr_pval))

  structure(
    list(
      model = ems@model.info,
      data = object$data,
      lsm_est = lsm_est_res,
      lsm_contr = contr_est_res,
      params = list(
        var = var,
        by = by,
        contrast = contrast,
        null = null,
        conf.level = conf.level,
        alternative = alternative
      )
    ),
    class = "s_lsmeans"
  )
}
