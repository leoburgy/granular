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
    checkboxInput("log", "log?"), 
    fileInput('file', "file"),
    actionButton("btn", "Go"),
    sliderInput("test2", "test2:", 0, 1, 0),
    dataTableOutput('temp')
  ),
  server = function(input,output,session) {
    getData <- reactive({
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
     
      dat <- read.csv(inFile$datapath)
      vals <- as.numeric(gsub("X", "", colnames(dat)[2:ncol(dat)]))
      if(input$log) return(log(vals))
      return(vals)
    })
    
    output$temp <- renderDataTable(getData())
    
    observeEvent(input$btn, {
      vals <- getData()
      vals <- vals[5:75]
      if(input$log) vals <- log(vals)
      vals <- round(vals, 1)
      
      
      js$slideVals("test1", vals)
    })
  }
))
