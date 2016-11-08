library(shiny)
library(ggvis)
source('global.R')

mastersizer_vis <- function(outputId) {
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg width=500 height=500> </svg></div>", sep=""))
}

shinyUI(fluidPage(
  
  tags$head(
    tags$script(src="//d3js.org/d3.v4.js"),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css'),
    tags$script('Shiny.addCustomMessageHandler("myCallbackHandler",
                  function(typeMessage) {console.log(typeMessage)
                    if(typeMessage == 1){
                          console.log("got here");
                          $("a:contains(Selected Data)").click();
                          }
                    if(typeMessage == 2){
                          $("a:contains(Selected Data)").click();
                          }
                          });')),
  
  # Application title
  titlePanel("Starch Granule Mixture Classification"),
  
  #sidebarPanel for data upload, estimation of mu
  tags$div(class = 'col-sm-4',
           tags$form(class = 'well',
                     tags$div(class = 'row-fluid',
                              fileInput('file', 'Choose CSV file', accept=c('text/csv', 
                                                                            'text/comma-separated-values,text/plain', 
                                                                            '.csv')))
                     # uiOutput("starchPar"),
                     # uiOutput("peakmu")
                     )
  ),
  actionButton("goButton", "Go!"),
  
  mainPanel(
    
    #tabsetPanel for logic flow
    tabsetPanel(
      
      #First tabPanel for visualisation of data to estimate number of mixtures and where the centres are
      tabPanel(
        "Initial inspection",
        includeHTML("www/index.html"),
        mastersizer_vis("mastersizer")
        #ggvisOutput("p2")
        #         ,
        #         uiOutput("p_ui")
      ),
      tabPanel(
        "Selected Data",
        shiny::dataTableOutput("longDataTable")
      )
    )
  )
))
