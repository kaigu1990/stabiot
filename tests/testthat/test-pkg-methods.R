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
