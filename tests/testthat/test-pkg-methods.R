test_that("print.s_lsmeans works as expected for mmrm", {
  data(fev_data, package = "mmrm")
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    reml = TRUE, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear",
    data = fev_data
  )
  res <- capture_output(print(s_get_lsmeans(fit, "ARMCD", by = "AVISIT")))
  expect_match(res, "Model Call: mmrm::mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT", fixed = TRUE)
  expect_match(res, "LS Mean (SE)", fixed = TRUE)
  expect_match(res, "Difference LS Mean (SE)", fixed = TRUE)

  res2 <- capture_output(print(s_get_lsmeans(fit, "ARMCD", by = "AVISIT", null = 2, alternative = "greater")))
  expect_match(res2, "p-value (Superiority Test)", fixed = TRUE)

  res3 <- capture_output(print(s_get_lsmeans(fit, "ARMCD", by = "AVISIT", null = -2, alternative = "greater")))
  expect_match(res3, "p-value (Non-inferiority Test)", fixed = TRUE)
})

test_that("print.prop_ci works as expected", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 150, TRUE),
    trtp = factor(rep(c("TRT1", "TRT2", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 150, TRUE)),
    strata2 = factor(sample(c("C", "D"), 150, TRUE)),
    strata3 = factor(sample(c("E", "F"), 150, TRUE))
  )
  res <- capture_output(print(s_propci(dta, var = "orr", by = "trtp")))
  expect_match(res, "PBO            TRT1            TRT2", fixed = TRUE)
  expect_match(res, "Proportion of response", fixed = TRUE)
  expect_match(res, "95% CI (clopper-pearson)", fixed = TRUE)
  expect_match(res, "Difference in response", fixed = TRUE)
  expect_match(res, "95% CI (wald)", fixed = TRUE)
  expect_match(res, "p-value (Chi-Square Test)", fixed = TRUE)

  res2 <- capture_output(print(s_propci(
    dta,
    var = "orr", by = "trtp",
    by.level = c("TRT1", "TRT2", "PBO")
  )))
  expect_match(res2, "TRT1           TRT2             PBO", fixed = TRUE)

  res3 <- capture_output(print(s_propci(
    dta,
    var = "orr", by = "trtp",
    method = "wald",
    diff.method = "score"
  )))
  expect_match(res3, "95% CI (wald)", fixed = TRUE)
  expect_match(res3, "95% CI (score)", fixed = TRUE)
})

test_that("print.or_ci works as expected", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 150, TRUE),
    trtp = factor(rep(c("TRT1", "TRT2", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 150, TRUE)),
    strata2 = factor(sample(c("C", "D"), 150, TRUE)),
    strata3 = factor(sample(c("E", "F"), 150, TRUE))
  )
  res <- capture_output(print(s_odds_ratio(
    dta,
    var = "orr", by = "trtp", or.method = "wald"
  )))
  expect_match(res, "PBO         TRT1           TRT2", fixed = TRUE)
  expect_match(res, "Unstratified Analysis", fixed = TRUE)
  expect_match(res, "Odds Ratio", fixed = TRUE)
  expect_match(res, "95% CI (wald)", fixed = TRUE)
  expect_match(res, "p-value (Fisher's Exact Test)", fixed = TRUE)

  res2 <- capture_output(print(s_odds_ratio(
    dta,
    var = "orr", by = "trtp",
    strata = c("strata1", "strata2", "strata3"),
    strata.method = "CMH",
    correct = FALSE
  )))
  expect_match(res2, "Stratified Analysis", fixed = TRUE)
  expect_match(res2, "95% CI (CMH)", fixed = TRUE)
  expect_match(res2, "p-value (CMH Test)", fixed = TRUE)

  res3 <- capture_output(print(s_odds_ratio(
    dta,
    var = "orr", by = "trtp",
    or.glm = TRUE,
    strata = c("strata1", "strata2", "strata3"),
    strata.method = "clogit"
  )))
  expect_match(res3, "95% CI (clogit)", fixed = TRUE)
  expect_match(res3, "p-value (clogit Test)", fixed = TRUE)
})

test_that("print.s_survival works as expected", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )
  res <- capture_output(print(s_get_survfit(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)))
  expect_match(res, "Surv formula: Surv(LENFOL, FSTAT) ~ AFB", fixed = TRUE)
  expect_match(res, "Group by: 1, 0", fixed = TRUE)
  expect_match(res, "Confidence interval type: log-log", fixed = TRUE)
  expect_match(res, "Time to event (months)", fixed = TRUE)
  expect_match(res, "Unstratified log-rank test", fixed = TRUE)

  res2 <- capture_output(print(s_get_survfit(data = dat, formula = Surv(LENFOL, FSTAT) ~ 1)))
  expect_match(res2, "Group by: Total", fixed = TRUE)

  res3 <- capture_output(print(s_get_survfit(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    time_point = c(12, 36, 60)
  )))
  expect_match(res3, "Event-free rate", fixed = TRUE)
  expect_match(res3, "Difference in Event Free Rate", fixed = TRUE)

  res4 <- capture_output(print(s_get_survfit(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    strata = c("AGE", "GENDER")
  )))
  expect_match(res4, "Stratified by: AGE, GENDER", fixed = TRUE)
})

test_that("print.s_coxph works as expected", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )
  res <- capture_output(print(s_get_coxph(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)))
  expect_match(res, "Surv formula: Surv(LENFOL, FSTAT) ~ AFB", fixed = TRUE)
  expect_match(res, "Group by: 1, 0", fixed = TRUE)
  expect_match(res, "Tie method: efro", fixed = TRUE)
  expect_match(res, "P-value method for HR: logtest, sctest, waldtest", fixed = TRUE)
  expect_match(res, "Unstratified Analysis", fixed = TRUE)
  expect_match(res, "Hazard Ratio", fixed = TRUE)
  expect_match(res, "p-value (logtest)", fixed = TRUE)
  expect_match(res, "p-value (sctest)", fixed = TRUE)
  expect_match(res, "p-value (waldtest)", fixed = TRUE)

  res2 <- capture_output(print(s_get_coxph(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    strata = c("AGE", "GENDER")
  )))
  expect_match(res2, "Stratified by: AGE, GENDER", fixed = TRUE)

  res3 <- capture_output(print(s_get_coxph(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    pval_method = "log"
  )))
  expect_match(res3, "P-value method for HR: logtest", fixed = TRUE)
})

test_that("print.count_evt works as expected", {
  data("rand_adsl")
  data("rand_adae")

  res <- capture_output(print(s_count_event(
    data = rand_adae, var = "SUBJID", by = "ARMCD",
    cond = list(
      "TEAEs" = c("TRTEMFL" = "Y"),
      "TRAEs" = c("TRTEMFL" = "Y", "AEREL" = "Y"),
      "SAE" = c("AESER" = "Y"),
      "TRSAE" = c("AESER" = "Y", "AEREL" = "Y")
    ),
    label = c(
      "Any TEAEs", "Any treatment-related TEAEs",
      "Any serious TEAEs", "Any serious treatment-related TEAEs"
    ),
    denom = rand_adsl
  )))
  expect_match(res, "ARM A        ARM B        ARM C", fixed = TRUE)
  expect_match(res, "(N=36)       (N=34)       (N=30)", fixed = TRUE)

  res2 <- capture_output(print(s_count_event(
    data = rand_adae, var = "SUBJID",
    cond = list(
      "TEAEs" = c("TRTEMFL" = "Y"),
      "TRAEs" = c("TRTEMFL" = "Y", "AEREL" = "Y")
    ),
    label = c("Any TEAEs", "Any treatment-related TEAEs"),
    denom = 200
  )))
  expect_match(res2, "Total", fixed = TRUE)
  expect_match(res2, "(N=200)", fixed = TRUE)
})
