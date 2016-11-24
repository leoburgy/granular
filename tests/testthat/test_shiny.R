library(granular)
library(shinytest)

context("shiny initialisation")

print(getwd())
print(list.files("../../granular"))
test_that("shiny app launches", {
  
  expect_error(app <- shinytest::shinyapp$new("granular"), NA)
})