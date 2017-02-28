library(granular)
library(dplyr)
library(purrr)
library(tidyr)
context("mix_grp_tbl")

data(package = 'granular')
means <- c(C = 1, B = 5, A = 22)

grp_tbl <- suppressWarnings(Dist %>% 
  gather(sample, proportion, -size) %>% 
  group_by(sample) %>% 
  mix_grp_tbl(proportion, size, means))

test_that("mix_grp_tbl is behaving", {
  expect_equal(grp_tbl %>% 
                 select(sample, mix_out) %>% 
                 unnest %>% 
                 filter(row_number() > 6) %>% 
                 data.frame %>% 
                 mutate_at(vars(sample), as.factor), 
               ms3, tolerance = 1e-05)
})