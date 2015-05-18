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
    if (is.null(input$logOption))
      return(NULL)
    
    wideData <- read.csv(inFile$datapath)
    longData <- gather(wideData, size, proportion, -sample) %>%
      mutate(size = sub("X", "", size))
    longData$size <- as.numeric(longData$size)
    longData <- filter(longData, size > input$minSize,
                       size < input$maxSize)
    return(longData)
  })
  
  getLimits <- reactive({
    dat <- getData()
    if (is.null(dat))
      return(NULL)
    ymax <- max(dat$proportion, na.rm = TRUE) * 1.2
  })
  
  observe({
    longData <- getData()
    
    if (is.null(getData()))
      return(NULL)
    
    if (is.null(input$logOption))
      return(NULL)
    
    if (!input$logOption) {
      p2 <- longData %>%
      group_by(sample) %>%
      ggvis(~size, ~proportion) %>%
      layer_lines(opacity := input_slider(0, 1, value = 1))
    }
    
    if (input$logOption) {
      p2 <- longData %>%
      group_by(sample) %>%
      ggvis(~size, ~proportion) %>%
      layer_paths(opacity := input_slider(0, 1, value = 1, label = "Change transparency")) %>%
      scale_numeric("x", trans = "log", expand = 0, nice = FALSE)
    }
    
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
  
  output$starchPar <- renderUI({
    if (is.null(input$file))
      return(NULL)
    uiout <- tags$div(class = 'row-fluid',
                      tags$div(class = 'row half-gutter',
                               tags$div(class = 'col-sm-6', 
                                        numericInput('minSize', HTML(paste0('Set the minimum granule size (', '&mu;', 'm)')), 0)
                               ),
                               tags$div(class = 'col-sm-6',
                                        numericInput('maxSize', HTML(paste0('Set the maximum granule size (', '&mu;', 'm)')), 1000)
                               )),
                      tags$div(class = 'row half-gutter',
                               tags$div(class = 'col-sm-6', 
                                        numericInput('peakNumber', 'Number of peaks', min = 2, value = NULL)
                               ),
                               tags$div(class = 'col-sm-6',
                                        checkboxInput('logOption', 'Log transform granule size', value = TRUE)
                               )))
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
        uilist[[i]] <- list(tags$div(class = 'row-fluid',
                                     tags$div(class = 'row half-gutter',
                                              tags$div(class = 'col-sm-3', textInput(paste0("peakid", i), "Enter an ID for this peak")),
                                              tags$div(class = 'col-sm-9', sliderInput(inputId = paste0("peak", i), 
                                                                                       label = HTML(paste0("Estimated mean for peak ", i, " (", "&mu;", "m)")), #  "\\((\\mu m\\))")), 
                                                                                       min = ifelse(xmin == 0, 1e-6, xmin), 
                                                                                       max = xmax, 
                                                                                       value = ifelse(xmin == 0, 0.1, xmin),
                                                                                       step = 0.1, 
                                                                                       round = FALSE,
                                                                                       width = '100%'))
                                     )
        ))
      }
      uiout <- tags$form(class = 'col-sm-12', uilist)
      return(uiout)
    }
  })  
})
