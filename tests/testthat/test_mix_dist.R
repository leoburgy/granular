library(granular)
context("mix_dist")

test_that("mix_dist is behaving", {
  expect_equal(mix_dist(Dist[[2]], Dist[[1]], means, names(Dist)[2]), 
               ms1)
  expect_equal(mix_dist(Dist[[3]], Dist[[1]], means, names(Dist)[3]), 
               ms2)
  expect_equal(mix_dist(Dist[[4]], Dist[[1]], means, names(Dist)[4]), 
               ms3)
})