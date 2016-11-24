library(granular)
library(shinytest)

context("shiny initialisation")

test_that("shiny app launches", {
  app <- shinytest::shinyapp$new("granular")
  expect_false(app$get_value("use_example"))
})