# s_propci ----

test_that("s_propci works as expected with default arguments", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 100, TRUE),
    trtp = factor(rep(c("TRT", "PBO"), each = 50))
  )

  res <- s_propci(dta, var = "orr", by = "trtp")
  expect_class(res, "prop_ci")
  expect_equal(
    res$prop_est,
    tibble::tibble(
      group = factor(c("PBO", "TRT"), levels = c("PBO", "TRT")),
      est = c(0.44, 0.40),
      lwr.ci = c(0.2999072, 0.2640784),
      upr.ci = c(0.5874559, 0.5482060)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$prop_diff,
    tibble::tibble(
      group = "PBO - TRT",
      est = 0.04,
      lwr.ci = -0.1533125,
      upr.ci = 0.2333125
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params,
    list(
      var = "orr",
      by = "trtp",
      by.level = c("PBO", "TRT"),
      event = 1,
      conf.level = 0.95,
      method = "clopper-pearson",
      diff.method = "wald",
      alternative = "two.sided"
    )
  )
})

test_that("s_propci works as expected with specific group level and event", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 100, TRUE),
    trtp = factor(rep(c("TRT", "PBO"), each = 50))
  )

  res <- s_propci(dta, var = "orr", by = "trtp", by.level = c("TRT", "PBO"), event = 0)
  expect_equal(
    res$prop_est,
    tibble::tibble(
      group = factor(c("TRT", "PBO"), levels = c("TRT", "PBO")),
      est = c(0.6, 0.56),
      lwr.ci = c(0.4517940, 0.4125441),
      upr.ci = c(0.7359216, 0.7000928)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$prop_diff,
    tibble::tibble(
      group = "TRT - PBO",
      est = 0.04,
      lwr.ci = -0.1533125,
      upr.ci = 0.2333125
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params[c("by.level", "event")],
    list(
      by.level = c("TRT", "PBO"),
      event = 0
    )
  )
})

test_that("s_propci works as expected with different method for binomial CI", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 100, TRUE),
    trtp = factor(rep(c("TRT", "PBO"), each = 50))
  )

  res <- s_propci(dta, var = "orr", by = "trtp", method = "wald")
  expect_equal(
    res$prop_est,
    tibble::tibble(
      group = factor(c("PBO", "TRT"), levels = c("PBO", "TRT")),
      est = c(0.44, 0.40),
      lwr.ci = c(0.3024111, 0.2642097),
      upr.ci = c(0.5775889, 0.5357903)
    ),
    tolerance = 0.0001
  )
  expect_equal(res$params$method, "wald")
})

# s_odds_ratio ----

test_that("s_odds_ratio works as expected with default arguments, no stratification", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 100, TRUE),
    trtp = factor(rep(c("TRT", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 100, TRUE)),
    strata2 = factor(sample(c("C", "D"), 100, TRUE)),
    strata3 = factor(sample(c("E", "F"), 100, TRUE))
  )

  res <- s_odds_ratio(dta, var = "orr", by = "trtp")
  expect_class(res, "or_ci")
  expect_null(res$strata_or)
  expect_equal(
    res$or,
    tibble::tibble(
      or.est = 0.8484848,
      lwr.ci = 0.3831831,
      upr.ci = 1.8788054
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params,
    list(
      var = "orr",
      by = "trtp",
      by.level = c("PBO", "TRT"),
      event = 1,
      strata = NULL,
      conf.level = 0.95,
      or.method = "wald",
      strata.method = NA
    )
  )
})

test_that("s_odds_ratio works as expected with stratification", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 100, TRUE),
    trtp = factor(rep(c("TRT", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 100, TRUE)),
    strata2 = factor(sample(c("C", "D"), 100, TRUE)),
    strata3 = factor(sample(c("E", "F"), 100, TRUE))
  )

  res <- s_odds_ratio(
    dta,
    var = "orr", by = "trtp",
    strata = c("strata1", "strata2", "strata3")
  )
  expect_equal(
    res$strata_or,
    tibble::tibble(
      or.est = 0.7499121,
      lwr.ci = 0.3248143,
      upr.ci = 1.7313529,
      pval = 0.5089342
    ),
    tolerance = 0.0001
  )
  expect_equal(res$params$strata.method, "CMH")
})

test_that("s_odds_ratio works as expected using or.glm and clogit", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 100, TRUE),
    trtp = factor(rep(c("TRT", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 100, TRUE)),
    strata2 = factor(sample(c("C", "D"), 100, TRUE)),
    strata3 = factor(sample(c("E", "F"), 100, TRUE))
  )

  res <- s_odds_ratio(
    dta,
    var = "orr", by = "trtp",
    or.glm = TRUE,
    strata = c("strata1", "strata2", "strata3"),
    strata.method = "clogit"
  )
  expect_equal(
    res$or,
    tibble::tibble(
      or.est = 0.8484848,
      lwr.ci = 0.3811997,
      upr.ci = 1.8797355
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$strata_or,
    tibble::tibble(
      data.frame(
        or.est = 0.7592608,
        lwr.ci = 0.335024,
        upr.ci = 1.720704,
        row.names = "TRT"
      )
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params[c("or.method", "strata.method")],
    list(
      or.method = "logit",
      strata.method = "clogit"
    )
  )
})
