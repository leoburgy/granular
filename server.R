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
    return(dfOut)
  })
  
  getLimits <- reactive({
    
    dat <- getData()
    if (is.null(dat))
      return(NULL)
    
    ymax <- max(dat$proportion, na.rm = TRUE) * 1.2
  })
  
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
      vals <- sapply(1:n, function(i) {
        as.numeric(input[[paste0("peak", i)]])[1]
      })
      peakDat <- data.frame(x = rep(vals, 2), 
                            y = rep(c(0, ymax), each = n), 
                            peak = rep(1:n, 2))
      p2 <- p2 %>%
        layer_paths(data = group_by(peakDat, peak), ~x, ~y, stroke := "red")
    }
    bind_shiny(p2, "p2", "p_ui")
    
    
  })
  
  output$temp <- renderText({
    if (is.null(input$peak1))
      return(NULL)
    
    n <- input$peakNumber
    ymax <- getLimits()
    vals <- sapply(1:n, function(i) {
      as.numeric(input[[paste0("peak", i)]])[1]
    })
    return(vals)
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
        uilist[[i]] <- sliderInput(inputId = paste0("peak", i), 
                                   label = #withMathJax(
                                     HTML(paste0("Estimated mean for peak ", i, " (", "&mu;", "m)")), #  "\\((\\mu m\\))")), 
                                   min = ifelse(xmin == 0, 1e-6, xmin), 
                                   max = xmax, 
                                   value = ifelse(xmin == 0, 0.1, xmin),
                                   step = 0.1, 
                                   round = FALSE)
      }
      return(uilist)
    }
  })
  
})
