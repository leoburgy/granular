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
                     tags$div(class = 'row-fluid',
                     fileInput('file', 'Choose CSV file', accept=c('text/csv', 
                                                                   'text/comma-separated-values,text/plain', 
                                                                   '.csv'))),
                     uiOutput("starchPar"),
                     uiOutput("peakmu"))
  ),
  
  mainPanel(
    
    #tabsetPanel for logic flow
    tabsetPanel(
      
      #First tabPanel for visualisation of data to estimate number of mixtures and where the centres are
      tabPanel(
        "Initial inspection",
        ggvisOutput("p2")
#         ,
#         uiOutput("p_ui")
      )
    )
  )
))
