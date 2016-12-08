# library(shinytest)

if(suppressMessages(suppressWarnings(require("shinytest", quietly = TRUE)))) {
  context("shinyapp")
  app <- shinytest::shinyapp$new("../../granular/shiny/granular/")
  
  test_that("get_value", {
    expect_false(app$get_value("use_example"))
  })
}
