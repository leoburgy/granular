library(granular)
context("mix_dist")

data(package = 'granular')
means <- c(C = 1, B = 5, A = 22)

test_that("mix_dist is behaving", {
  expect_equal(mix_dist(Dist[[2]], Dist[[1]], means, names(Dist)[2])[[1]], 
               ms1, tolerance = 1e-05)
  expect_equal(mix_dist(Dist[[3]], Dist[[1]], means, names(Dist)[3])[[1]], 
               ms2, tolerance = 1e-05)
  expect_equal(mix_dist(Dist[[4]], Dist[[1]], means, names(Dist)[4])[[1]], 
               ms3, tolerance = 1e-05)
})