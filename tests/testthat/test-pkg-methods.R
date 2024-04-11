test_that("print.s_lsmeans works as expected for mmrm", {
  data(fev_data, package = "mmrm")
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    reml = TRUE, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear",
    data = fev_data
  )
  res <- capture_output(print(s_get_lsmeans(fit, "ARMCD", by = "AVISIT")))
  expect_match(res, "Model Call: mmrm::mmrm(formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT", fixed = TRUE)
  expect_match(res, "Predictor/Treatment: ARMCD (PBO, TRT)", fixed = TRUE)
  expect_match(res, "Group by: AVISIT (VIS1, VIS2, VIS3, VIS4)", fixed = TRUE)
  expect_match(res, "Least-squares Means Estimates", fixed = TRUE)
  expect_match(res, "Null hypothesis is θ equal to 0", fixed = TRUE)

  res2 <- capture_output(print(s_get_lsmeans(fit, "ARMCD", by = "AVISIT", null = 2, alternative = "greater")))
  expect_match(res2, "Null hypothesis is θ non-superiority to 2", fixed = TRUE)

  res3 <- capture_output(print(s_get_lsmeans(fit, "ARMCD", by = "AVISIT", null = -2, alternative = "greater")))
  expect_match(res3, "Null hypothesis is θ inferiority to -2", fixed = TRUE)
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
