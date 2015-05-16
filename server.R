library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggvis)

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
  
  getLimits <- reactive({
    
    dat <- getData()
    if (is.null(dat))
      return(NULL)
    
    ymax <- max(dat$proportion, na.rm = TRUE) * 1.2
  })
  # 
  #   output$initialPlot <- renderPlot({
  #     longData <- getData()
  #     if (is.null(longData))
  #       return(NULL)
  #     
  #     p <- ggplot(longData, aes(size, proportion)) + geom_path(aes(group = sample))
  #     if (input$logOption)
  #       p <- p + scale_x_log10()
  #     
  #     return(p)
  #   })
  
  observe({
    longData <- getData()
    
    if (is.null(longData))
      return(NULL)
    if (!input$logOption)
      p2 <- longData %>%
        group_by(sample) %>%
        ggvis(~size, ~proportion) %>%
        layer_lines(opacity := input_slider(0, 1, value = 1))
    
    if (input$logOption)
      p2 <- longData %>%
        group_by(sample) %>%
        ggvis(~size, ~proportion) %>%
        layer_paths(opacity := input_slider(0, 1, value = 1, label = "Change transparency")) %>%
        scale_numeric("x", trans = "log", expand = 0, nice = FALSE)
    
    
    if (!is.null(input$peak1)) {
      n <- input$peakNumber
      ymax <- getLimits()
      peakVals <- names(input)[grepl("peak[0-9]+", names(input))]
#      xseq <- with(input, lapply(peakVals, get))
#       peakDat <- data.frame(x = rep(xseq, 2), y = rep(c(0, ymax), each = n), peak = rep(1:n, 2)) %>%
#         group_by(peak)
      peakDat <- data.frame(x = rep(input$peak1, 2), y = c(0, ymax), peak = rep(1, 2))
      p2 <- p2 %>%
        layer_paths(data = peakDat, ~x, ~y, stroke := "red")
  }
    bind_shiny(p2, "p2", "p_ui")
    
    
  })

  output$peakmu <- renderUI({
    if (is.null(input$peakNumber))
      return(NULL)
    
    
    if (!is.na(input$peakNumber)){
      n <- input$peakNumber
      xmin <- input$minSize
      xmax <- input$maxSize
      xseq <- seq(xmin, xmax, by = (xmax - xmin) / (n + 2))
      xseq <- xseq[2:(n + 1)]
      uilist <- vector(mode = "list", n)
      for(i in 1:n) {
        uilist[[i]] <- numericInput(paste0("peak", i), paste("Estimated mean for peak", i), input$minSize, input$maxSize, 0)
      }
      return(uilist)
    }
  })
  
})
