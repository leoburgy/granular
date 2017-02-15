library(granular)
context("mix_dist")

data(package = 'granular')
means <- c(C = 1, B = 5, A = 22)

means_nosort <- means[c(2,3,1)]

means_nonames <- setNames(means, NULL)

test_that("mix_dist is behaving", {
  expect_equal(mix_dist(Dist[[2]], Dist[[1]], means, names(Dist)[2])[[1]], 
               ms1, tolerance = 1e-05)
  expect_equal(suppressWarnings(mix_dist(Dist[[3]], Dist[[1]], means, names(Dist)[3]))[[1]], 
               ms2, tolerance = 1e-05)
  expect_equal(mix_dist(Dist[[4]], Dist[[1]], means, names(Dist)[4])[[1]], 
               ms3, tolerance = 1e-05)
})

test_that("sorting means works", {
  expect_error(mix_dist(Dist[[2]], Dist[[1]], means, names(Dist)[2]), NA)
  expect_error(mix_dist(Dist[[2]], Dist[[1]], means_nosort, names(Dist)[2]), NA)
})

test_that("no mean names is OK", {
  expect_error(mix_dist(Dist[[2]], Dist[[1]], means_nonames, names(Dist)[2]), NA)
  expect_message(mix_dist(Dist[[2]], Dist[[1]], means_nonames, names(Dist)[2]), "No names supplied")
})