test_that("rrPostProb works as expected", {
  set.seed(12306)
  res <- rrPostProb(nsample = 66, cutoff = 0.75, p = c(0.75, 0.8, 0.85, 0.9), succs = 0.7)
  expect_equal(res, c(29.0, 66.4, 93.7, 99.8))
})
