library(granular)
context("ggfit")

fit <- ggfit(ms3, Dist[[4]], Dist[[1]], c(C = 1, B = 5, A = 22))

test_that("ggfit is behaving", {
  expect_equal_to_reference(fit, "fit.rds")
})
