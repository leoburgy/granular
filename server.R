library(shiny)
library(tidyr)
library(dplyr)
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
    
    params <- list(range = list(min_val = min_val, max_val = max_val), 
                   peaks = list(peak_A = peak_A, peak_B = peak_B, peak_C = peak_C))
    
    print(params)
    
    print(params)
    
    return(params)
      
  })
  
  output$params <- renderText({
    params <- get_params()
    paste("Min/max are: ", params[[1]], "\n",
          "Peaks are: ", params[[2]])
  })
  
  
  getFitData <- reactive({
    print("running getFitData")
    wideData <- getData()
    params <- get_params()
    if(!is.null(getData())) {
      ps <- wideData[, 1]
      Dist <- wideData[, 2:ncol(wideData)]
      eg.out <- mixDist(ps, Dist, comp_means = rev(unlist(params[[2]])))
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
  
  output$longDataTable <- renderDataTable({
    outputData()
  })
  
})
