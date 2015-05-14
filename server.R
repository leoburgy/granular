library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  getData <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    wideData <- read.csv(inFile$datapath)
    longData <- gather(wideData, size, proportion, -sample) %>%
      mutate(size = sub("X", "", size))
    longData$size <- as.numeric(longData$size)
    dfOut <- filter(longData, size > input$minSize,
                    size < input$maxSize)
#     longData <- longData %>%
#       group_by(sample) %>%
#       mutate(index = row_number())
#     df_index <- longData %>%
#       filter(proportion > 0) %>%
#       mutate(first = min(index),
#              last = max(index))
#     dfOut <- full_join(longData, df_index) %>%
#       filter(row_number() >= max(first, na.rm = TRUE),
#              row_number() <= max(last, na.rm = TRUE))
    return(dfOut)
    
  })

  output$initialPlot <- renderPlot({
    longData <- getData()
    if (is.null(longData))
      return(NULL)
    
    p <- ggplot(longData, aes(size, proportion)) + geom_path(aes(group = sample))
    
    if (input$logOption)
      p <- p + scale_x_log10()
    
    return(p)
      
  })

})
