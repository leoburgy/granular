library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggvis)
library(scales)
source('global.R')

shinyServer(function(input, output, session) {
  
  observe({
    if(input$goButton > 0){
      print('1')
      session$sendCustomMessage("myCallbackHandler", "1")
    }
  })
# observe({
#   if(input$action1 > 0){
#     print('2')
#     session$sendCustomMessage("myCallbackHandler", "2")
#   }
# })
  # 
#   plotTheme <- theme(panel.grid = element_blank(),
#                      panel.background = element_rect(fill = "#F8F8F8"))
#   
  getData <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    # if (is.null(input$logOption))
    #   return(NULL)
    
    wideData <- read.csv(inFile$datapath)
    tData <- gather(wideData, size, proportion, -sample) %>%
      mutate(size = as.numeric(sub("X", "", size))) %>% 
      spread(sample, proportion)
    #longData$size <- as.numeric(longData$size)
    # if(!is.na(input$spuriousPeak)) {
    #   sizes <- unique(longData$size)
    #   nearest <- sizes[which.min(abs(sizes - input$spuriousPeak))]
    #   goodSamples <- longData$sample[longData$size == nearest & longData$proportion == 0]
    #   longData <- longData %>%
    #     filter(sample %in% goodSamples, size) 
    # }
    
#     longData <- filter(longData, size > input$minSize,
#                        size < input$maxSize)
    return(tData)
  })
  
  output$mastersizer <- reactive({
    getData()
  })
  
  get_params <- reactive({
    print("getting params again")
    min_val <- if(!is.null(input$min_val)) {
      input$min_val
    } else NULL
    
    max_val <- if(!is.null(input$max_val)) {
      input$max_val
    } else NULL
    
    npeaks <- if(!is.null(input$npeaks)) {
      input$npeaks
    } else NULL
    
    peak_vals <- if(!is.null(npeaks)) {
      peak_ids <- LETTERS[1:npeaks]
      peak_vals <- eval(parse(paste0("input$peak_", peak_ids)))
    }
    
    peak_A <- if(!is.null(input$peak_A)) {
      input$peak_A
    } else(NULL)
    
    peak_B <- if(!is.null(input$peak_B)) {
      input$peak_B
    } else(NULL)
    
    peak_C <- if(!is.null(input$peak_C)) {
      input$peak_C
    } else(NULL)
    
    params <- list(min_val = min_val, max_val = max_val, 
                   peak_A = peak_A, peak_B = peak_B, peak_C = peak_C)
    
    print(params)
    
    return(params)
      
  })
  
  output$params <- renderText({
    params <- get_params()
    paste("Parameters are: ", params[[1]], params[[2]])
  })
  
  # getLimits <- reactive({
  #   if (is.null(getData()))
  #     return(NULL)
  #   longData <- getData()
  #   limits <- longData %>% 
  #     group_by(size) %>%
  #     summarise(measured = any(proportion > 0, na.rm = TRUE)) %>%
  #     mutate(first = !duplicated(measured), last = rev(!duplicated(rev(measured))))
  #   
  #   min <- limits %>% filter(measured, first) %>%
  #     select(size)
  #   min <- min$size
  #   max <- limits %>% filter(measured, last)
  #   max <- max$size
    
#     if(!is.na(input$minSize))
#       min <- input$minSize
#     if(!is.na(input$maxSize))
#       max <- input$maxSize
  #   return(list(xmin = min, xmax = max))
  # })
  
  # getSpurious <- reactive({
  #   if(is.null(input$spuriousPeak))
  #     return(NULL)
  #   if(is.na(input$spuriousPeak))
  #     return(NA)
  #   return(input$spuriousPeak)
  # })
  # 
  # filteredData <- reactive({
  #   if (is.null(getData()))
  #     return(NULL)
  #   getSpurious()
  #   longData <- getData()
  #   limits <- getLimits()
  #   longData <- longData %>%
  #     filter(size >= limits[[1]], size <= limits[[2]])
  #   })
  
  
  
  # getVals <- reactive({
  #   longData <- filteredData()
  #   limits <- getLimits()
  #   n <- input$peakNumber
  #   
  #   ymax <- max(longData$proportion, na.rm = TRUE) * 1.2
  #   
  #   if (is.na(input$peakNumber))
  #     return(NULL)
  #   
  #   if (!is.na(input$peakNumber) & input$peakNumber > 0){
  #     
  #     vals <- sapply(1:n, function(i) {
  #       as.numeric(input[[paste0("peak", i)]])[1]
  #     })
  #     
  #     labels <- sapply(1:n, function(i) {
  #       as.character(input[[paste0("peakid", i)]])[1]
  #     })
  #     offset <- log((limits[[2]] - limits[[1]])/20)
  #     
  #     peakDat <- data.frame(x = vals, peak = 1:n)
  #     peakDat$label <- ""
  #     if (any(!is.na(labels)))
  #       peakDat$label <- labels
  #     peakDat$xoff <- with(peakDat, x - x * offset)
  #     peakDat$ylabel <- ymax / 1.2
  #     return(peakDat)
  #   }
  # })
  
  # output$ggplot <- renderPlot({
  #   if(is.null(getData()))
  #     return(NULL)
  #   longData <- filteredData()
  #   limits <- getLimits()
  #   
  #   
  #   p2 <- ggplot(longData, aes(size, proportion)) + geom_line(aes(group = sample)) + ylab("Proportion (%)") +
  #     xlab("Granule size")
  #   
  #   if (input$logOption) {
  #     p2 <- p2 + scale_x_continuous(trans = 'log10',
  #                                   breaks = trans_breaks('log10', function(x) 10^x),
  #                                   labels = comma_format(digits = 0)) + 
  #       annotation_logticks(sides = "b")
  #   }
  #   
  #   if(!is.null(input$peak1) & !is.na(input$peakNumber)) {
  #     if(input$peak1 > 0) {
  #       peakDat <- getVals()
  #       p2 <- p2 + geom_vline(data = peakDat, aes(xintercept = x), colour = "red") 
  #       if(any(!is.na(peakDat$label))) {
  #         p2 <- p2 + geom_text(data = peakDat, aes(x = x, y = ylabel, label = label))
  #       }
  #     }
  #   }
  #   return(p2 + plotTheme)
  # })
  # 
  # output$starchPar <- renderUI({
  #   if (is.null(input$file))
  #     return(NULL)
  #   limits <- getLimits()
  #   spurious <- getSpurious()
  #   uiout <- tags$div(class = 'row-fluid',
  #                     tags$div(class = 'row half-gutter',
  #                              tags$div(class = 'col-sm-6', 
  #                                       numericInput('minSize', HTML(paste0('Set the minimum granule size (', '&mu;', 'm)')), limits[[1]])
  #                              ),
  #                              tags$div(class = 'col-sm-6',
  #                                       numericInput('maxSize', HTML(paste0('Set the maximum granule size (', '&mu;', 'm)')), limits[[2]])
  #                              )),
  #                     tags$div(class = 'row half-gutter',
  #                              tags$div(class = 'col-sm-12',
  #                                       numericInput('spuriousPeak', 'Spurious peak size', spurious))),
  #                     tags$div(class = 'row half-gutter',
  #                              tags$div(class = 'col-sm-6', 
  #                                       numericInput('peakNumber', 'Number of peaks', min = 2, value = NULL)
  #                              ),
  #                              tags$div(class = 'col-sm-6',
  #                                       checkboxInput('logOption', 'Log transform granule size', value = TRUE)
  #                              )))
  #   return(uiout)
  # })
  # 
  # output$peakmu <- renderUI({
  #   #       Would like to make the slider inputs on a log scale
  #   #       The code here (https://groups.google.com/forum/#!searchin/shiny-discuss/slider%7Csort:date/shiny-discuss/AeAzR4p2h1g/8vWpRDgqnTUJ)
  #   #       would probably help, but I can't get it to work atm      
  #   if (is.null(input$peakNumber)) {
  #     return(NULL)
  #   }
  #   
  #   if (!is.na(input$peakNumber) & input$peakNumber > 0){
  #     n <- input$peakNumber
  #     xmin <- input$minSize
  #     xmax <- input$maxSize
  #     xseq <- seq(xmin, xmax, by = (xmax - xmin) / (n + 2))
  #     xseq <- xseq[2:(n + 1)]
  #     uilist <- vector(mode = "list", n)
  #     for(i in seq_len(n)) {
  #       mu <- isolate(ifelse(is.null(input[[paste0('peak', i)]]), 
  #                            ifelse(xmin == 0, 0.1, xmin),
  #                            ifelse(as.numeric(input[[paste0('peak', i)]])[1] > xmax, 
  #                                   xmax,
  #                                   as.numeric(input[[paste0('peak', i)]])[1])))
  #       uilist[[i]] <- list(tags$div(class = 'row-fluid',
  #                                    tags$div(class = 'row half-gutter',
  #                                             tags$div(class = 'col-sm-3', textInput(paste0("peakid", i), "Peak ID", i)),
  #                                             tags$div(class = 'col-sm-9', sliderInput(inputId = paste0("peak", i), 
  #                                                                                      label = HTML(paste0("Estimated mean for peak ", i, " (", "&mu;", "m)")), #  "\\((\\mu m\\))")), 
  #                                                                                      min = ifelse(xmin == 0, 1e-6, xmin), 
  #                                                                                      max = xmax, 
  #                                                                                      value = mu,
  #                                                                                      step = 0.1, 
  #                                                                                      round = FALSE,
  #                                                                                      width = '100%'))
  #                                    )
  #       ))
  #     }
  #     uiout <- tags$form(class = 'col-sm-12', uilist)
  #     return(uiout)
  #   }
  # })
  
  getWideData <- reactive({
    print("running getWideData")
    longData <- getData()
    if(!is.null(getData())) {
      wideData <- longData %>% 
        spread(sample, proportion) %>% 
        setNames(c("particle_size", paste0("freq", 1:(ncol(.) - 1))))
    }
  })
  
  getFitData <- reactive({
    print("running getFitData")
    wideData <- getWideData()
    if(!is.null(getWideData())) {
      ps <- wideData[, 1]
      Dist <- wideData[, 2:ncol(wideData)]
      eg.out <- mixDist(ps, Dist)
      eg.out
    }
  })
  
  outputData <- eventReactive(input$goButton, {
    print("running outputData")
    fitData <- getFitData()
    if(!is.null(getFitData())) {
      cat(str(fitData))
      return(fitData)
    }
  })
  
  
  
  output$temp <- renderText(paste(input$spuriousPeak, paste(getLimits())))
  output$longDataTable <- renderDataTable({
    outputData()
  })
  
  
})
