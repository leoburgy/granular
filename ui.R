library(shiny)
library(ggvis)

shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css')),
  
  # Application title
  titlePanel("Starch Granule Mixture Classification"),
  
  #sidebarPanel for data upload, estimation of mu
  tags$div(class = 'col-sm-4',
           tags$form(class = 'well',
                     fileInput('file', 'Choose CSV file', accept=c('text/csv', 
                                                                   'text/comma-separated-values,text/plain', 
                                                                   '.csv')),
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
                              ))),
           uiOutput("peakmu")
  ),
  
  mainPanel(
    
    #tabsetPanel for logic flow
    tabsetPanel(
      
      #First tabPanel for visualisation of data to estimate number of mixtures and where the centres are
      tabPanel(
        "Initial inspection",
        ggvisOutput("p2"),
        uiOutput("p_ui")
      )
    )
  )
))
