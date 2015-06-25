library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggvis)
library(scales)

shinyServer(function(input, output, session) {
  
  observe({
    if(input$goButton > 0){
      print('1')
      session$sendCustomMessage("myCallbackHandler", "1")
    }
  })
#   observe({
#     if(input$action1 > 0){
#       print('2')
#       session$sendCustomMessage("myCallbackHandler", "2")
#     }
#   })
#   
  plotTheme <- theme(panel.grid = element_blank(),
                     panel.background = element_rect(fill = "#F8F8F8"))
  
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
    if (is.null(getData()))
      return(NULL)
    xmin <- input$minSize
    xmax <- input$maxSize
    return(list(xmin = xmin, xmax = xmax))
  })
  
  getVals <- reactive({
    longData <- getData()
    limits <- getLimits()
    n <- input$peakNumber
    
    ymax <- max(longData$proportion, na.rm = TRUE) * 1.2
    
    if (is.na(input$peakNumber))
      return(NULL)
    
    if (!is.na(input$peakNumber) & input$peakNumber > 0){
      
      vals <- sapply(1:n, function(i) {
        as.numeric(input[[paste0("peak", i)]])[1]
      })
      
      labels <- sapply(1:n, function(i) {
        as.character(input[[paste0("peakid", i)]])[1]
      })
      offset <- log((limits$xmax - limits$xmin)/20)
      
      peakDat <- data.frame(x = vals, peak = 1:n)
      peakDat$label <- ""
      if (any(!is.na(labels)))
        peakDat$label <- labels
      peakDat$xoff <- with(peakDat, x - x * offset)
      peakDat$ylabel <- ymax / 1.2
      return(peakDat)
    }
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
    
    if(!is.null(input$peak1) & !is.na(input$peakNumber)) {
      if(input$peak1 > 0) {
        peakDat <- getVals()
        p2 <- p2 + geom_vline(data = peakDat, aes(xintercept = x), colour = "red") 
        if(any(!is.na(peakDat$label))) {
          p2 <- p2 + geom_text(data = peakDat, aes(x = x, y = ylabel, label = label))
        }
      }
    }
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
    return(uiout)
  })
  
  output$peakmu <- renderUI({
    #       Would like to make the slider inputs on a log scale
    #       The code here (https://groups.google.com/forum/#!searchin/shiny-discuss/slider%7Csort:date/shiny-discuss/AeAzR4p2h1g/8vWpRDgqnTUJ)
    #       would probably help, but I can't get it to work atm      
    if (is.null(input$peakNumber)) {
      return(NULL)
    }
    
    if (!is.na(input$peakNumber) & input$peakNumber > 0){
      n <- input$peakNumber
      xmin <- input$minSize
      xmax <- input$maxSize
      xseq <- seq(xmin, xmax, by = (xmax - xmin) / (n + 2))
      xseq <- xseq[2:(n + 1)]
      uilist <- vector(mode = "list", n)
      for(i in seq_len(n)) {
        mu <- isolate(ifelse(is.null(input[[paste0('peak', i)]]), 
                             ifelse(xmin == 0, 0.1, xmin),
                             ifelse(as.numeric(input[[paste0('peak', i)]])[1] > xmax, 
                                    xmax,
                                    as.numeric(input[[paste0('peak', i)]])[1])))
        uilist[[i]] <- list(tags$div(class = 'row-fluid',
                                     tags$div(class = 'row half-gutter',
                                              tags$div(class = 'col-sm-3', textInput(paste0("peakid", i), "Peak ID", i)),
                                              tags$div(class = 'col-sm-9', sliderInput(inputId = paste0("peak", i), 
                                                                                       label = HTML(paste0("Estimated mean for peak ", i, " (", "&mu;", "m)")), #  "\\((\\mu m\\))")), 
                                                                                       min = ifelse(xmin == 0, 1e-6, xmin), 
                                                                                       max = xmax, 
                                                                                       value = mu,
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
  
  
  outputData <- eventReactive(input$goButton, {
    longData <- getData()
    if(!is.null(getData()))
      return(longData)
    
  })
  
  output$longDataTable <- renderDataTable({
    outputData()
  })
})
