test_that("s_count_event works as expected with default arguments", {
  data("rand_adsl")
  data("rand_adae")

  res <- s_count_event(
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
  )

  expect_equal(
    res$cnt,
    tibble::tibble(
      group = factor(rep(c("ARM A", "ARM B", "ARM C"), 4), levels = c("ARM A", "ARM B", "ARM C")),
      n = c(34, 30, 26, 33, 28, 25, 25, 23, 22, 23, 19, 17),
      N = rep(c(36, 34, 30), 4),
      perc = c(
        0.9444444, 0.8823529, 0.8666667, 0.9166667, 0.8235294, 0.8333333,
        0.6944444, 0.6764706, 0.7333333, 0.6388889, 0.5588235, 0.5666667
      ),
      label_ = rep(c("TEAEs", "TRAEs", "SAE", "TRSAE"), each = 3),
      label = rep(c(
        "Any TEAEs", "Any treatment-related TEAEs",
        "Any serious TEAEs", "Any serious treatment-related TEAEs"
      ), each = 3)
    ),
    tolerance = 0.0001
  )
})

test_that("s_count_event works as expected with default arguments", {
  data("rand_adsl")
  data("rand_adae")

  res <- s_count_event(
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
  )

  expect_equal(
    res$cnt,
    tibble::tibble(
      group = factor(rep(c("ARM A", "ARM B", "ARM C"), 4), levels = c("ARM A", "ARM B", "ARM C")),
      n = c(34, 30, 26, 33, 28, 25, 25, 23, 22, 23, 19, 17),
      N = rep(c(36, 34, 30), 4),
      perc = c(
        0.9444444, 0.8823529, 0.8666667, 0.9166667, 0.8235294, 0.8333333,
        0.6944444, 0.6764706, 0.7333333, 0.6388889, 0.5588235, 0.5666667
      ),
      label_ = rep(c("TEAEs", "TRAEs", "SAE", "TRSAE"), each = 3),
      label = rep(c(
        "Any TEAEs", "Any treatment-related TEAEs",
        "Any serious TEAEs", "Any serious treatment-related TEAEs"
      ), each = 3)
    ),
    tolerance = 0.0001
  )
})

test_that("s_count_event works as expected with specific the denominator", {
  data("rand_adsl")
  data("rand_adae")

  res <- s_count_event(
    data = rand_adae, var = "SUBJID", by = "ARMCD",
    cond = list("TEAEs" = c("TRTEMFL" = "Y")),
    label = c("Any TEAEs"),
    denom = c(100, 100, 100)
  )

  expect_equal(
    res$cnt,
    tibble::tibble(
      group = factor(c("ARM A", "ARM B", "ARM C"), levels = c("ARM A", "ARM B", "ARM C")),
      n = c(34, 30, 26),
      N = rep(100, 3),
      perc = c(0.34, 0.30, 0.26),
      label_ = rep("TEAEs", 3),
      label = rep("Any TEAEs", 3),
    )
  )
})

test_that("s_count_event works as expected with no grouping", {
  data("rand_adsl")
  data("rand_adae")

  res <- s_count_event(
    data = rand_adae, var = "SUBJID",
    cond = list(
      "TEAEs" = c("TRTEMFL" = "Y"),
      "TRAEs" = c("TRTEMFL" = "Y", "AEREL" = "Y")
    ),
    label = c("Any TEAEs", "Any treatment-related TEAEs"),
    denom = 200
  )

  expect_equal(
    res$cnt,
    tibble::tibble(
      group = factor("Total", levels = "Total"),
      n = c(90, 86),
      N = rep(200, 2),
      perc = c(0.45, 0.43),
      label_ = c("TEAEs", "TRAEs"),
      label = c("Any TEAEs", "Any treatment-related TEAEs"),
    )
  )
})
