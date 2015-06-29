library(shiny)
library(shinyjs)

jsCode <- '
shinyjs.slideVals = function(params) {
var defaultParams = {
id : null,
vals : [0, 1]
};
params = shinyjs.getParams(params, defaultParams);

$("#" + params.id).data("ionRangeSlider").update({"values":params.vals});
}'

runApp(shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    sliderInput("test1", "test1:", 0, 1, 0),
    numericInput("min", "Min", NULL),
    numericInput("max", "Max", NULL),
    numericInput("length", "Length", NULL),
    actionButton("btn", "Go"),
    sliderInput("test2", "test2:", 0, 1, 0)
  ),
  server = function(input,output,session) {
    observeEvent(input$btn, {
      vals <- seq(from = input$min, to = input$max, length.out = input$length)
      js$slideVals("test1", vals)
    })
  }
))