test_that("sfreq works as expected with default arguments", {
  data("rand_adsl")
  data("rand_adae")

  res <- sfreq(
    data = rand_adsl,
    var = "RACE",
    by = "TRT01P",
    fmt = "xx (xx.xx%)"
  )

  expect_equal(
    head(res$cnt_tb, 3),
    tibble::tibble(
      Label = factor(c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"), levels = levels(rand_adsl$RACE)),
      `A: Drug X` = c("21 (58.33%)", "7 (19.44%)", "7 (19.44%)"),
      `B: Placebo` = c("16 (47.06%)", "3 (8.82%)", "11 (32.35%)"),
      `C: Combination` = c("16 (53.33%)", "4 (13.33%)", "8 (26.67%)"),
      Total = c("53 (53.00%)", "14 (14.00%)", "26 (26.00%)"),
      DTYPE = c("VAR", "VAR", "VAR"),
      VARNAME = c("RACE", "RACE", "RACE"),
      RACE = factor(c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"), levels = levels(rand_adsl$RACE))
    )
  )
  expect_equal(dim(res$cnt_num), c(8, 21))
  expect_equal(
    res$params[-6],
    list(
      var = "RACE",
      sub_var = NULL,
      nested_vars = NULL,
      by = "TRT01P",
      fmt = "xx (xx.xx%)",
      distinct_vars = "USUBJID",
      bymap = data.frame(
        TRT01P = c("A: Drug X", "B: Placebo", "C: Combination", "Total"),
        number = c("1", "2", "3", "4")
      )
    )
  )
})

test_that("sfreq works as expected when nested_vars is SEX", {
  data("rand_adsl")
  data("rand_adae")

  res <- sfreq(
    data = rand_adsl,
    var = "RACE",
    nested_vars = "SEX",
    by = "TRT01P",
    fmt = "xx (xx.xx%)",
    nested_row = TRUE
  )

  expect_equal(
    head(res$cnt_tb, 4),
    tibble::tibble(
      Label = factor(c("F", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"), levels = c(levels(rand_adsl$RACE), "F", "M")),
      `A: Drug X` = c("16 (44.44%)", "11 (30.56%)", "2 (5.56%)", "3 (8.33%)"),
      `B: Placebo` = c("18 (52.94%)", "7 (20.59%)", "2 (5.88%)", "7 (20.59%)"),
      `C: Combination` = c("16 (53.33%)", "9 (30.00%)", "0 (0.00%)", "7 (23.33%)"),
      Total = c("50 (50.00%)", "27 (27.00%)", "4 (4.00%)", "17 (17.00%)"),
      DTYPE = c("NESTED_VARS", "VAR", "VAR", "VAR"),
      VARNAME = c("SEX", "RACE", "RACE", "RACE"),
      RACE = factor(c(NA, "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE"), levels = levels(rand_adsl$RACE)),
      SEX = factor(c("F", "F", "F", "F"), levels = levels(rand_adsl$SEX))
    )
  )
  expect_equal(dim(res$cnt_num), c(18, 23))
  expect_equal(res$params$nested_vars, "SEX")
})

test_that("sfreq works as expected when row_tot is n (%)", {
  data("rand_adsl")
  data("rand_adae")

  res <- sfreq(
    data = rand_adsl,
    var = "RACE",
    by = "TRT01P",
    fmt = "xx (xx.xx%)",
    row_tot = "n (%)"
  )

  expect_equal(
    head(res$cnt_tb, 5),
    tibble::tibble(
      Label = c("n (%)", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE"),
      `A: Drug X` = c("36 (100.00%)", "21 (58.33%)", "7 (19.44%)", "7 (19.44%)", "1 (2.78%)"),
      `B: Placebo` = c("34 (100.00%)", "16 (47.06%)", "3 (8.82%)", "11 (32.35%)", "4 (11.76%)"),
      `C: Combination` = c("30 (100.00%)", "16 (53.33%)", "4 (13.33%)", "8 (26.67%)", "1 (3.33%)"),
      Total = c("100 (100.00%)", "53 (53.00%)", "14 (14.00%)", "26 (26.00%)", "6 (6.00%)"),
      DTYPE = c("VAR", "VAR", "VAR", "VAR", "VAR"),
      VARNAME = c("RACE", "RACE", "RACE", "RACE", "RACE"),
      RACE = factor(c("n (%)", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE"),
        levels = c("n (%)", "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "OTHER")
      )
    )
  )
  expect_equal(dim(res$cnt_num), c(6, 21))
})

test_that("sfreq works as expected for SOC and PT", {
  data("rand_adsl")
  data("rand_adae")
  
  res <- sfreq(
    data = rand_adae,
    var = "AEDECOD",
    nested_vars = "AEBODSYS",
    by = "TRT01P",
    fmt = "xx (xx.x%)",
    denom = rand_adsl,
    fctdrop = TRUE,
    nested_row = TRUE,
    .order = "desc(perc4)"
  )
  
  expect_equal(
    head(res$cnt_tb, 3),
    tibble::tibble(
      Label = factor(
        c("cl B.2", "dcd B.2.1.2.1", "dcd B.2.2.3.1"),
        levels = c(levels(rand_adae$AEDECOD), levels(rand_adae$AEBODSYS))
      ),
      `A: Drug X` = c("22 (61.1%)", "16 (44.4%)", "11 (30.6%)"),
      `B: Placebo` = c("20 (58.8%)", "12 (35.3%)", "14 (41.2%)"),
      `C: Combination` = c("17 (56.7%)", "11 (36.7%)", "12 (40.0%)"),
      Total = c("59 (59.0%)", "39 (39.0%)", "37 (37.0%)"),
      DTYPE = c("NESTED_VARS", "VAR", "VAR"),
      VARNAME = c("AEBODSYS", "AEDECOD", "AEDECOD"),
      AEDECOD = factor(
        c(NA, "dcd B.2.1.2.1", "dcd B.2.2.3.1"),
        levels = c(
          "dcd C.1.1.1.3", "dcd B.2.1.2.1", "dcd D.1.1.1.1", "dcd B.1.1.1.1", "dcd D.1.1.4.2", 
          "dcd B.2.2.3.1", "dcd C.2.1.2.1", "dcd D.2.1.5.3", "dcd A.1.1.1.1", "dcd A.1.1.1.2"
        )
      ),
      AEBODSYS = factor(c("cl B.2", "cl B.2", "cl B.2"), levels = levels(rand_adae$AEBODSYS))
    )
  )
  expect_equal(dim(res$cnt_num), c(17, 23))
})
