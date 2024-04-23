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
      reference = "PBO",
      comparison = "TRT",
      est = -0.04,
      lwr.ci = -0.2333125,
      upr.ci = 0.1533125,
      pval = 0.6853155
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params,
    list(
      var = "orr",
      by = "trtp",
      by.level = c("PBO", "TRT"),
      resp = 1,
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

  res <- s_propci(dta, var = "orr", by = "trtp", by.level = c("TRT", "PBO"), resp = 0)
  expect_equal(
    res$prop_est,
    tibble::tibble(
      group = factor(c("TRT", "PBO"), levels = c("TRT", "PBO")),
      est = c(0.60, 0.56),
      lwr.ci = c(0.4517940, 0.4125441),
      upr.ci = c(0.7359216, 0.7000928)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$prop_diff,
    tibble::tibble(
      reference = "TRT",
      comparison = "PBO",
      est = -0.04,
      lwr.ci = -0.2333125,
      upr.ci = 0.1533125,
      pval = 0.6853155
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params[c("by.level", "resp")],
    list(
      by.level = c("TRT", "PBO"),
      resp = 0
    )
  )
})

test_that("s_propci works as expected with three groups", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 150, TRUE),
    trtp = factor(rep(c("TRT1", "TRT2", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 150, TRUE)),
    strata2 = factor(sample(c("C", "D"), 150, TRUE)),
    strata3 = factor(sample(c("E", "F"), 150, TRUE))
  )

  res <- s_propci(dta, var = "orr", by = "trtp")
  expect_equal(
    res$prop_est,
    tibble::tibble(
      group = factor(c("PBO", "TRT1", "TRT2"), levels = c("PBO", "TRT1", "TRT2")),
      est = c(0.50, 0.40, 0.44),
      lwr.ci = c(0.3552730, 0.2640784, 0.2999072),
      upr.ci = c(0.6447270, 0.5482060, 0.5874559)
    ),
    tolerance = 0.0001
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
      group = "TRT",
      or.est = 0.8484848,
      lwr.ci = 0.3831831,
      upr.ci = 1.8788054,
      pval = 0.8395904
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$params,
    list(
      var = "orr",
      by = "trtp",
      by.level = c("PBO", "TRT"),
      resp = 1,
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
      group = "TRT",
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
      group = "TRT",
      or.est = 0.8484848,
      lwr.ci = 0.3811997,
      upr.ci = 1.879735,
      pval = 0.6854057
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$strata_or,
    tibble::tibble(
      data.frame(
        group = "TRT",
        or.est = 0.7592608,
        lwr.ci = 0.335024,
        upr.ci = 1.720704,
        pval = 0.5093958
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

test_that("s_odds_ratio works as expected with three groups", {
  set.seed(12)
  dta <- data.frame(
    orr = sample(c(1, 0), 150, TRUE),
    trtp = factor(rep(c("TRT1", "TRT2", "PBO"), each = 50)),
    strata1 = factor(sample(c("A", "B"), 150, TRUE)),
    strata2 = factor(sample(c("C", "D"), 150, TRUE)),
    strata3 = factor(sample(c("E", "F"), 150, TRUE))
  )

  res <- s_odds_ratio(dta,
    var = "orr",
    strata = c("strata1", "strata2", "strata3"),
    by = "trtp"
  )
  expect_equal(
    res$or,
    tibble::tibble(
      group = c("TRT1", "TRT2"),
      or.est = c(0.6666667, 0.7857143),
      lwr.ci = c(0.3019255, 0.3577195),
      upr.ci = c(1.472034, 1.725785),
      pval = c(0.4215518, 0.6888527)
    ),
    tolerance = 0.0001
  )
  expect_equal(
    res$strata_or,
    tibble::tibble(
      data.frame(
        group = c("TRT1", "TRT2"),
        or.est = c(0.8992580, 0.8132399),
        lwr.ci = c(0.3817818, 0.3570678),
        upr.ci = c(2.118134, 1.852195),
        pval = c(0.8131148, 0.6223752)
      )
    ),
    tolerance = 0.0001
  )
})
