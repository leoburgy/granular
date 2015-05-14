library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Starch Granule Mixture Classification"),
  
  #sidebarPanel for data upload, estimation of mu
  sidebarPanel(
    fileInput('file', 'Choose CSV file', accept=c('text/csv', 
                                                  'text/comma-separated-values,text/plain', 
                                                  '.csv')),
    numericInput('minSize', 'Set the minimum granule size', 0),
    numericInput('maxSize', 'Set the maximum granule size', 1000),
    checkboxInput('logOption', 'Log transform granule size', value = TRUE)
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
