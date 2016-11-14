library(shiny)
library(tidyr)
library(dplyr)
library(scales)
library(shinyjs)
# source('../../R/granular.R')

shinyServer(function(input, output, session) {
  
  observe({
    if(input$goButton > 0){
      print('1')
      session$sendCustomMessage("myCallbackHandler", "1")
    }
  })

  observe({
    toggle('select_data', condition = input$use_example)
    toggle('file', condition = !input$use_example)
    toggle('download_example', condition = input$use_example)
  })
  
  output$download_example <- downloadHandler(
    filename = switch(input$select_data,
                      "Single sample" = "singleMastersizer.csv",
                      "Three samples" = "tripleMastersizer.csv",
                      "Thirty-six samples" = "extdata/fullMastersizer.csv"),
    content = function(file) {
      write.csv(read.csv(example_file(), check.names = FALSE), file, row.names = FALSE)
    }
  )
  
  example_file <- reactive({
    switch(input$select_data,
           "Single sample" = "../../extdata/singleMastersizer.csv",
           "Three samples" = "../../extdata/tripleMastersizer.csv",
           "Thirty-six samples" = "../../extdata/fullMastersizer.csv")
  })
  
  getData <- reactive({
    if(!input$use_example) {
      inFile <- input$file$datapath
    } else {
      print(input$select_data)
      inFile <- example_file()
    }
    
    if(is.null(inFile)) return(NULL)
    
    wideData <- read.csv(inFile)
    tData <- gather(wideData, size, proportion, -sample) %>%
      mutate(size = as.numeric(sub("X", "", size))) %>% 
      spread(sample, proportion)
    print(paste(dim(tData)))
    return(tData)
  })
  
  observe({
    if(!is.null(getData())) {
      output_list <<- vector("list", ncol(getData()) - 1)
      
    }
  })
  
  output$mastersizer <- reactive({
    getData()
  })
  
  get_params <- reactive({
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
    return(params)
  })
  
  observeEvent(input$goButton, {
    wideData <- getData()
    params <- get_params()
    means <- rev(unlist(params[[2]]))
    tData <- getData()
    ps <- tData[[1]]
    withProgress({
      n <- ncol(tData)
      for(i in seq_along(2:n)) {
        incProgress(1/(n - 1), 
                    "Calculating...", 
                    paste("working on", 
                          names(tData)[i + 1],
                          "which is", 
                          i, "of", n - 1)
        )
        newfit <- granular::mix_dist(tData[[i + 1]], ps, 
                           names(tData)[i + 1], comp_means = means)
        output_list[[i]] <<- newfit[[1]]
        output_df <<- bind_rows(output_list)
      }
    })
  })
  
  output$longDataTable <- renderDataTable({
    if(exists("output_list")) {
      # print("does output_list exist?")
      # print(exists("output_list"))
      # print(output_list)
      output_df# <- bind_rows(output_list)
      # print("output_df: ")
      # print(output_df)
      # return(output_df)
    }
  })
  
})