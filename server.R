library(shiny)
library(tidyr)

shinyServer(function(input, output) {
  
  getData <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    wideData <- read.csv(inFile$datapath)
    longData <- gather(wideData, size, proportion, -sample)
  })

  output$table <- renderTable({
    getData()
    
  })

})
