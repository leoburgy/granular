library(shiny)
library(tidyr)
library(dplyr)
library(scales)
library(shinyjs)
library(ggplot2)
library(granular)
# source('../../../R/granular.R')

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  granular <- reactiveValues()
  
  observe({
    granular$version <- sessionInfo()
  })
  
  
  output$granular_version <- renderText({
    paste0("granular - v", granular$version$otherPkgs$granular$Version)
  })
  
  observe({
    toggle(id = 'select_data', condition = input$use_example)
    toggle(id = 'file', condition = !input$use_example)
    toggle(id = 'download_example', condition = input$use_example)
    params <- values$params
    
    #Set class toggle for instruction text
    toggleClass('step1', "instgrey", !is.null(getData()))
    toggleClass('step2', "instgrey", (is.null(getData()) | !any(as.logical(lapply(params[[1]], is.null)))))
    toggleClass('step3', "instgrey", (is.null(getData()) | any(as.logical(lapply(params[[1]], is.null))) | all(!as.logical(lapply(params[[2]], is.null)))))
    
    #Set toggle for the go button
    toggleState('goButton', condition = !any(as.logical(lapply(params[[2]], is.null))))
  })
  
  observe({
    #Hide tabs to start
    hide(selector = "#tabset li a[data-value=output]")
    hide(selector = "#tabset li a[data-value=summary]")
    hide(selector = "#tabset li a[data-value=downloads]")
  })
  
  observe({
    input$restartButton
    isolate({
      reset("sidePanel")
      reset("goButton")
      values$params <- list(range = list(min_val = NULL, max_val = NULL), 
                            peaks = list(peak_A = NULL, peak_B = NULL, peak_C = NULL))
      updateTabsetPanel(session, "tabset", "setup")
      hide(selector = "#tabset li a[data-value=output]")
      hide(selector = "#tabset li a[data-value=summary]")
      hide(selector = "#tabset li a[data-value=downloads]")
    })
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
  
  getFile <- reactive({
    input$restartButton
    if(!input$use_example) {
      inFile <- input$file$datapath
    } else {
      inFile <- example_file()
    }
    
    if(is.null(inFile)) return(NULL)
    return(inFile)
  })
  
  getData <- reactive({
    inFile <- getFile()
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
  
  observe({
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
    
    values$params <- list(range = list(min_val = min_val, max_val = max_val), 
                          peaks = list(peak_A = peak_A, peak_B = peak_B, peak_C = peak_C))
  })
  
  filteredData <- reactive({
    if(is.null(values$params[[1]][[1]])) {
      return(NULL)
    } 
    
    if(is.null(getData())) return(NULL)
    
    params <- values$params
    tData <- getData()
    
    outData <- tData %>%
      gather(sample, proportion, -size) %>%
      filter(size > params[[1]][[1]],
             size < params[[1]][[2]]) %>%
      spread(sample, proportion)
    return(outData)
  })
  
  observe({
    if(input$goButton == 0) 
      return(NULL)
    isolate({
      tData <- filteredData()
      params <- values$params
      means <- rev(unlist(params[[2]]))
      ps <- tData[[1]]
      n <- ncol(tData)
      output_list <- vector("list", n - 1)
      withProgress({
        for(i in seq_len(n - 1)) {
          incProgress(1/n, 
                      "Calculating...", 
                      paste("working on", 
                            names(tData)[i + 1],
                            "which is", 
                            i, "of", n - 1)
          )
          newfit <- granular::mix_dist(tData[[i + 1]], ps, mu_vec = means,
                                       sample_name = names(tData)[i + 1])
          output_list[[i]] <- newfit[[1]]
        }
      })
      values$output_list <- output_list
      values$output_table <- bind_rows(output_list)
      toggle(selector = "#tabset li a[data-value=output]")
      toggle(selector = "#tabset li a[data-value=summary]")
      toggle(selector = "#tabset li a[data-value=downloads]")
      updateTabsetPanel(session, "tabset", "output")
      
    })
  })
  
  output$longDataTable <- renderDataTable({
    values$output_table
  })
  
  output$downloadTable <- downloadHandler(
    filename = "granular_output_table.csv",
    content = function(file) {
      write.csv(values$output_table, file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = "granular_plots.zip",
    content = function(file) {
      if(is.null(values$output_list)) {
        return(NULL)
      } else {
        output_list <- values$output_list
        output_plots <- vector("list", length(output_list))
        tmpdir <- tempdir()
        tData <- filteredData()
        ps <- tData[[1]]
        for(i in seq_len(length(output_list))) {
          output_plots[[i]] <- paste0(names(tData)[[i + 1]], ".png")
          ggsave(paste0(tmpdir, "/", output_plots[[i]]), 
                 granular:::ggfit(output_list[[i]], 
                                  tData[[i + 1]],
                                  ps, 
                                  title = names(tData)[i + 1]), 
                 device = "png",
                 width = 6, height = 6)
        }
        wd <- getwd()
        setwd(tmpdir)
        zip(file, paste0(unlist(output_plots)))
        setwd(wd)
      }
    }
  )
})

