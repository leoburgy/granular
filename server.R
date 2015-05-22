library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggvis)
library(scales)

shinyServer(function(input, output, session) {
  
  plotTheme <- theme(panel.grid = element_blank(),
                     panel.background = element_rect(fill = "#F8F8F8"))
  
  values <- reactiveValues(starting = TRUE)
  
  session$onFlushed(function() {
    values$starting <- FALSE
  })
  
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
    cat("getData just ran\n")
    return(longData)
  })
  
  getLimits <- reactive({
    if (is.null(getData()))
      return(NULL)
    xmin <- input$minSize
    xmax <- input$maxSize
    cat("getLimits just ran\n")
    return(list(xmin = xmin, xmax = xmax))
  })
  
  getVals <- reactive({
    cat("getVals is starting\n")
    longData <- getData()
    limits <- getLimits()
    n <- input$peakNumber
    cat("getVals #1\n")
    ymax <- max(longData$proportion, na.rm = TRUE) * 1.2
    
    if (is.na(input$peakNumber))
      return(NULL)
    
    cat("getVals #2\n")
    if (!is.na(input$peakNumber) & input$peakNumber > 0){
      cat("getVals #2.5\n")
      if (!input$peakNumber > 0) {
        return(NULL)
      }
      
      if (exists("input$peak1")) {
        cat("getVals #3\n")
      vals <- sapply(1:n, function(i) {
        as.numeric(input[[paste0("peak", i)]])[1]
      })
      cat("getVals #4")
      labels <- sapply(1:n, function(i) {
        cat("getVals #5\n")
        as.character(input[[paste0("peakid", i)]])[1]
      })
      cat("getVals#5\n")
      offset <- log((limits$xmax - limits$xmin)/20)
      
      peakDat <- data.frame(x = vals, peak = 1:n)
      peakDat$label <- NA
      if (any(!is.na(labels)))
        peakDat$label <- labels
      peakDat$xoff <- with(peakDat, x - x * offset)
      peakDat$ylabel <- ymax / 1.2
      cat("getVals just ran and peakDat$x = ", peakDat$x, "\n")
      return(peakDat)
    }}
  })
  
  output$ggplot <- renderPlot({
    if(is.null(getData()))
      return(NULL)
    longData <- getData()
    limits <- getLimits()
    
    
    p2 <- ggplot(longData, aes(size, proportion)) + geom_line(aes(group = sample)) + ylab("Proportion (%)") +
      xlab("Granule size")
    
    if (input$logOption) {
      p2 <- p2 + scale_x_continuous(trans = 'log10',
                                    breaks = trans_breaks('log10', function(x) 10^x),
                                    labels = comma_format(digits = 0)) + 
        annotation_logticks(sides = "b")
    }
    
#     if(!is.null(input$peak1) & !is.na(input$peakNumber)) {
#       cat("as part of plot running, peakNumber is ", input$peakNumber, "\n")
#       if(input$peak1 > 0) {
#         peakDat <- getVals()
#         p2 <- p2 + geom_vline(data = peakDat, aes(xintercept = x), colour = "red") 
#         if(any(!is.na(peakDat$label))) {
#           p2 <- p2 + geom_text(data = peakDat, aes(x = x, y = ylabel, label = label))
#         }
#       }
#     }
    cat("plot just ran\n")
    return(p2 + plotTheme)
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
    cat("starchPar just ran\n")
    return(uiout)
  })
  
  
  output$peakmu <- renderUI({
    if (is.null(input$peakNumber))
      return(NULL)
#     if (is.null(getVals()))
#       return(NULL)
    
    if (!is.na(input$peakNumber) & input$peakNumber > 0){
      if (!input$peakNumber > 0) {
        return(NULL)
      }
      vals <- isolate(getVals())
      n <- input$peakNumber
      xmin <- input$minSize
      xmax <- input$maxSize
      xseq <- seq(xmin, xmax, by = (xmax - xmin) / (n + 2))
      xseq <- xseq[2:(n + 1)]
      uilist <- vector(mode = "list", n)
      for(i in seq_len(n)) {
        uilist[[i]] <- list(tags$div(class = 'row-fluid',
                                     tags$div(class = 'row half-gutter',
                                              tags$div(class = 'col-sm-3', textInput(inputId = paste0("peakid", i), 
                                                                                     label = "Peak ID", 
                                                                                     value = ifelse(is.na(vals$label[i]),
                                                                                                    i,
                                                                                                    vals$label[i]))),
                                              tags$div(class = 'col-sm-9', sliderInput(inputId = paste0("peak", i), 
                                                                                       label = HTML(paste0("Estimated mean for peak ", i, " (", "&mu;", "m)")), #  "\\((\\mu m\\))")), 
                                                                                       min = ifelse(xmin == 0, 1e-6, xmin), 
                                                                                       max = xmax, 
                                                                                       value = ifelse(is.na(vals$x[i]),
                                                                                                      ifelse(xmin == 0, 0.1, xmin),
                                                                                                      ifelse(vals$x[i] < xmax, vals$x[i], xmax)),
                                                                                       step = 0.1, 
                                                                                       round = FALSE,
                                                                                       width = '100%'))
                                     )
        ))
      }
      uiout <- tags$form(class = 'col-sm-12', uilist)
      cat("peakmu just ran\n")
      
        return(uiout)
      
    }
  })  
})
