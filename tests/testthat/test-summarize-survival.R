# s_get_survfit ----

test_that("s_get_survfit works as expected with default arguments in single group", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )

  res <- s_get_survfit(data = dat, formula = Surv(LENFOL, FSTAT) ~ 1)
  expect_class(res, "s_survival")
  expect_equal(
    res$surv$median,
    tibble::tibble(
      group = "Total",
      n = 500,
      events = 215,
      median = 53.4538,
      lower = 49.47844,
      upper = 77.30595
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$surv$quantile,
    tibble::tibble(
      group = rep("Total", 2),
      quantile = c(25, 75),
      time = c(9.724846, 77.305955),
      lower = c(4.796715, 77.207392),
      upper = c(13.33881, NA)
    ),
    tolerance = 0.0001
  )
  expect_identical(dim(res$surv$overall), c(395L, 9L))
  expect_equal(
    res$surv$range,
    tibble::tibble(
      group = "Total",
      event_min = 0.03285421,
      event_max = 77.47023,
      censor_min = 12.09035,
      censor_max = 72.01643,
      min = 0.03285421,
      max = 77.47023
    ),
    tolerance = 0.0001
  )
  expect_null(res$surv$time_point)
  expect_null(res$surv_diff$test)
  expect_null(res$surv_diff$rate)
})

test_that("s_get_survfit works as expected with default arguments in two groups", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )

  res <- s_get_survfit(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)
  expect_equal(
    res$surv$median,
    tibble::tibble(
      group = c("1", "0"),
      n = c(78, 422),
      events = c(47, 168),
      median = c(28.41889, 70.96509),
      lower = c(13.76591, 51.77823),
      upper = c(45.24025, NA)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$surv$quantile,
    tibble::tibble(
      group = rep(c("1", "0"), 2),
      quantile = rep(c(25, 75), each = 2),
      time = c(3.12115, 11.33470, 77.20739, 77.30595),
      lower = c(0.5585216, 6.1437372, 50.8583162, 77.3059548),
      upper = c(10.77618, 17.41273, NA, NA)
    ),
    tolerance = 0.0001
  )
  expect_null(res$surv$time_point)
  expect_equal(
    res$surv_diff$test,
    tibble::tibble(
      reference = "1",
      comparison = "0",
      method = "Log-Rank",
      pval = 0.0009616214
    ),
    tolerance = 0.0001
  )
  expect_null(res$surv_diff$rate)
})

test_that("s_get_survfit works as expected with specific time points", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )

  res <- s_get_survfit(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    time_point = c(12, 36, 60)
  )
  expect_equal(
    res$surv$time_point[, c("time", "surv", "lower", "upper")],
    tibble::tibble(
      time = rep(c(12, 36, 60), 2),
      surv = c(0.6410256, 0.4546027, 0.3147249, 0.7393365, 0.6446822, 0.5297982),
      lower = c(0.5241292, 0.3349104, 0.1950534, 0.6946720, 0.5946286, 0.4669984),
      upper = c(0.7363133, 0.5665873, 0.4414593, 0.7785306, 0.6902213, 0.5886077)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$surv_diff$rate[, c("surv.diff", "lower", "upper", "pval")],
    tibble::tibble(
      surv.diff = c(0.09831085, 0.19007956, 0.21507329),
      lower = c(-0.01608841, 0.06331489, 0.07509421),
      upper = c(0.2127101, 0.3168442, 0.3550524),
      pval = c(0.092118565, 0.003293759, 0.002600283)
    ),
    tolerance = 0.0001
  )
})

test_that("s_get_survfit works as expected with stratified variables", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )

  res <- s_get_survfit(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    strata = c("AGE", "GENDER")
  )
  expect_equal(
    res$surv_diff$test,
    tibble::tibble(
      reference = "1",
      comparison = "0",
      method = "Stratified Log-Rank",
      pval = 0.08269744
    ),
    tolerance = 0.0001
  )
})

test_that("s_get_survfit works as expected with three groups when pairwise is TRUE", {
  data("whas500")
  set.seed(123)
  subj <- sample(whas500$ID, 100)
  dat <- whas500 %>%
    mutate(
      AFB = case_when(
        ID %in% subj ~ 2,
        TRUE ~ AFB
      ),
      AFB = factor(AFB, levels = c(1, 2, 0))
    )

  res <- s_get_survfit(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    pairwise = TRUE
  )
  expect_identical(dim(res$surv$median), c(3L, 6L))
  expect_identical(dim(res$surv$quantile), c(6L, 5L))
  expect_identical(dim(res$surv$overall), c(445L, 9L))
  expect_identical(dim(res$surv$range), c(3L, 7L))
})

test_that("s_get_survfit works as expected when group variable is not factor", {
  data("whas500")
  dat <- whas500

  res <- s_get_survfit(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    strata = c("AGE", "GENDER")
  )
  expect_equal(
    res$surv_diff$test,
    tibble::tibble(
      reference = "1",
      comparison = "0",
      method = "Stratified Log-Rank",
      pval = 0.08269744
    ),
    tolerance = 0.0001
  )
})

# s_get_coxph ----

test_that("s_get_coxph works as expected with default arguments", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )

  res <- s_get_coxph(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)
  expect_equal(
    res$hr,
    tibble::tibble(
      reference = "1",
      comparison = "0",
      n = 500,
      events = 215,
      hr = 0.5828995,
      lower = 0.4214764,
      upper = 0.8061465
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$pval,
    tibble::tibble(
      reference = rep("1", 3),
      comparison = rep("0", 3),
      method = c("logtest", "sctest", "waldtest"),
      test = c(9.584563, 10.903404, 10.640000),
      df = c(1, 1, 1),
      pval = c(0.0019622016, 0.0009598769, 0.0011042969)
    ),
    tolerance = 0.0001
  )
})

test_that("s_get_coxph works as expected with stratification", {
  data("whas500")
  dat <- whas500 %>%
    mutate(
      AFB = factor(AFB, levels = c(1, 0))
    )

  res <- s_get_coxph(
    data = dat,
    formula = Surv(LENFOL, FSTAT) ~ AFB,
    strata = c("AGE", "GENDER")
  )
  expect_equal(
    res$hr[, c(3:7)],
    tibble::tibble(
      n = 500,
      events = 215,
      hr = 0.6952678,
      lower = 0.4620161,
      upper = 1.046278
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$pval[, 3:6],
    tibble::tibble(
      method = c("logtest", "sctest", "waldtest"),
      test = c(2.964049, 3.060662, 3.040000),
      df = c(1, 1, 1),
      pval = c(0.08513451, 0.08020901, 0.08133120)
    ),
    tolerance = 0.0001
  )
})

test_that("s_get_coxph works as expected when group variable is not factor", {
  data("whas500")
  dat <- whas500

  res <- s_get_coxph(data = dat, formula = Surv(LENFOL, FSTAT) ~ AFB)
  expect_equal(
    res$hr,
    tibble::tibble(
      reference = "1",
      comparison = "0",
      n = 500,
      events = 215,
      hr = 0.5828995,
      lower = 0.4214764,
      upper = 0.8061465
    ),
    tolerance = 0.0001
  )
})
