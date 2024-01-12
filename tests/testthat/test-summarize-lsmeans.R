test_that("s_get_lsmeans works as expected by visit with default arguments", {
  data(fev_data, package = "mmrm")
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    reml = TRUE, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear",
    data = fev_data
  )
  res <- s_get_lsmeans(fit, "ARMCD", by = "AVISIT")

  expect_class(res, "s_lsmeans")
  expect_identical(dim(res$lsm_est), c(8L, 9L))
  expect_identical(dim(res$lsm_contr), c(4L, 9L))
  expect_equal(
    res$lsm_est[1:2, ],
    tibble::tibble(
      ARMCD = factor(c("PBO", "TRT")),
      AVISIT = factor(c("VIS1", "VIS1"), levels = c("VIS1", "VIS2", "VIS3", "VIS4")),
      estimate = c(33.33186, 37.10609),
      SE = c(0.7612042, 0.7674844),
      df = c(148.1457, 143.1765),
      lower.CL = c(31.82764, 35.58903),
      upper.CL = c(34.83608, 38.62316),
      t.ratio = c(43.78833, 48.34768),
      p.value = c(1.165649e-86, 1.444629e-90)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$lsm_contr[1:2, ],
    tibble::tibble(
      contrast = factor(c("PBO - TRT", "PBO - TRT")),
      AVISIT = factor(c("VIS1", "VIS2"), levels = c("VIS1", "VIS2", "VIS3", "VIS4")),
      estimate = c(-3.774230, -3.732304),
      SE = c(1.0817635, 0.8633334),
      df = c(145.5520, 145.2771),
      lower.CL = c(-5.912224, -5.438620),
      upper.CL = c(-1.636236, -2.025988),
      t.ratio = c(-3.488960, -4.323131),
      p.value = c(6.417790e-04, 2.840654e-05)
    ),
    tolerance = 0.0001
  )
})

test_that("s_get_lsmeans works as expected for superiority testing with null hypothesis of 2", {
  data(fev_data, package = "mmrm")
  fev_data$ARMCD <- factor(fev_data$ARMCD, level = c("TRT", "PBO"))
  fit <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    reml = TRUE, method = "Kenward-Roger", vcov = "Kenward-Roger-Linear",
    data = fev_data
  )
  res <- s_get_lsmeans(fit, "ARMCD", by = "AVISIT", null = 2, alternative = "greater")

  expect_identical(dim(res$lsm_contr), c(4L, 10L))
  expect_equal(
    res$lsm_contr[1:2, ],
    tibble::tibble(
      contrast = factor(c("TRT - PBO", "TRT - PBO")),
      AVISIT = factor(c("VIS1", "VIS2"), levels = c("VIS1", "VIS2", "VIS3", "VIS4")),
      estimate = c(3.774230, 3.732304),
      SE = c(1.0817635, 0.8633334),
      df = c(145.5520, 145.2771),
      lower.CL = c(1.636236, 2.025988),
      upper.CL = c(5.912224, 5.438620),
      null = c(2, 2),
      t.ratio = c(1.640127, 2.006529),
      p.value = c(0.05156904, 0.02332779)
    ),
    tolerance = 0.0001
  )
})

test_that("s_get_lsmeans works as expected for ancova", {
  data(fev_data, package = "mmrm")
  fit <- fev_data %>%
    dplyr::filter(VISITN == 4 & !is.na(FEV1)) %>%
    lm(formula = FEV1 ~ FEV1_BL + RACE + SEX + ARMCD)
  res <- s_get_lsmeans(fit, "ARMCD")

  expect_identical(dim(res$lsm_est), c(2L, 8L))
  expect_identical(dim(res$lsm_contr), c(1L, 8L))
  expect_equal(
    res$lsm_est,
    tibble::tibble(
      ARMCD = factor(c("PBO", "TRT")),
      estimate = c(48.63345, 52.53288),
      SE = c(1.204272, 1.180020),
      df = c(128, 128),
      lower.CL = c(46.25060, 50.19801),
      upper.CL = c(51.01631, 54.86775),
      t.ratio = c(40.38412, 44.51864),
      p.value = c(1.071522e-74, 9.320133e-80)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$lsm_contr,
    tibble::tibble(
      contrast = c("PBO - TRT"),
      estimate = c(-3.89943),
      SE = c(1.686347),
      df = c(128),
      lower.CL = c(-7.236155),
      upper.CL = c(-0.562705),
      t.ratio = c(-2.312354),
      p.value = c(0.02235472)
    ),
    tolerance = 0.0001
  )
})
