library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Starch Granule Mixture Classification"),
  
  #sidebarPanel for data upload, estimation of mu
  sidebarPanel(
    fileInput('file', 'Choose CSV file', accept=c('text/csv', 
                                                  'text/comma-separated-values,text/plain', 
                                                  '.csv'))
  ),
  
  mainPanel(
    
    #tabsetPanel for logic flow
    tabsetPanel(
      
      #First tabPanel for visualisation of data to estimate number of mixtures and where the centres are
      tabPanel(
        "Initial inspection",
        tableOutput("table")
      )
    )
  )
))
