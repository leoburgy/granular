library(shiny)
library(tidyr)
library(dplyr)
library(scales)
library(shinyjs)
 source('../../../R/granular.R')

shinyServer(function(input, output, session) {
  
  observe({
    toggle('select_data', condition = input$use_example)
    toggle('file', condition = !input$use_example)
    toggle('download_example', condition = input$use_example)
    params <- get_params()
    
    #Set class toggle for instruction text
    toggleClass('step1', "instgrey", !is.null(getData()))
    toggleClass('step2', "instgrey", (is.null(getData()) | !any(as.logical(lapply(params[[1]], is.null)))))
    toggleClass('step3', "instgrey", (any(as.logical(lapply(params[[1]], is.null))) | all(!as.logical(lapply(params[[2]], is.null)))))
    
    #Set toggle for the go button
    toggleState('goButton', condition = !any(as.logical(lapply(params[[2]], is.null))))
    
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
      inFile <- example_file()
    }
    
    if(is.null(inFile)) return(NULL)
    
    wideData <- read.csv(inFile)
    tData <- gather(wideData, size, proportion, -sample) %>%
      mutate(size = as.numeric(sub("X", "", size))) %>% 
      spread(sample, proportion)
    return(tData)
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
  
  filteredData <- reactive({
    if(is.null(get_params()[[1]][[1]])) {
      return(NULL)
    } else {
      params <- get_params()
      tData <- getData()
      params <- get_params()
      
      outData <- tData %>%
        gather(sample, proportion, -size) %>%
        filter(size > params[[1]][[1]],
               size < params[[1]][[2]]) %>%
        spread(sample, proportion)
      return(outData)
    }
  })
  
  observe({filteredData()})
  
  observeEvent(input$goButton, {
    wideData <- filteredData()
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
        output$longDataTable <- renderDataTable({
          bind_rows(output_list)
        })
      }
    })
  })
})
