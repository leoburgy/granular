library(shiny)
library(shinyjs)

mastersizer_vis <- function(outputId) {
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"></div>", sep=""))
}

shinyUI(fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(src="//d3js.org/d3.v4.js"),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css')#,
    # tags$script('Shiny.addCustomMessageHandler("myCallbackHandler",
    #               function(typeMessage) {console.log(typeMessage)
    #                 if(typeMessage == 1){
    #                       console.log("got here");
    #                       $("a:contains(Selected Data)").click();
    #                       }
    #                 if(typeMessage == 2){
    #                       $("a:contains(Selected Data)").click();
    #                       }
    #                       });')
    ),
  
  # Application title
  titlePanel("Starch Granule Mixture Classification"),
  
  #sidebarPanel for data upload, estimation of mu
  tags$div(id = "sidePanel", class = 'col-sm-4',
           tags$form(class = 'well',
                     tags$div(class = 'row-fluid',
                              fileInput('file', 'Choose CSV file', accept=c('text/csv', 
                                                                            'text/comma-separated-values,text/plain', 
                                                                            '.csv'))),
                     tags$div(class = 'row-fluid',
                              selectInput("select_data", "Select data", c("Single sample", "Three samples", "Thirty-six samples"))),
                     tags$div(class = 'row-fluid',
                              checkboxInput('use_example', 'Use example data')),
                     tags$div(class = 'row-fluid',
                              downloadLink('download_example', "Download example data"))
                     ),
           tags$form(id = 'guide', class = 'well',
                     tags$div(h4("Guide:"), 
                              p(id = "step1", class = "inst", "1. Select Data"),
                              p(id = "step2", class = "inst", "2. Click and drag on the plot to identify the area with true peaks"),
                              p(id = "step3", class = "inst", "3. Click to identify peaks")),
                     actionButton("goButton", "4. Process Data", width = '100%')
           ),
           tags$form(id = 'restart', class = 'well',
                     actionButton("restartButton", "Start again", width = '100%'))
  ),
  mainPanel(
    #tabsetPanel for logic flow
    tabsetPanel(
      id = "tabset",
      
      #First tabPanel for visualisation of data to estimate number of mixtures and where the centres are
      tabPanel(title = "Setup",
        mastersizer_vis("mastersizer")
        ),
      tabPanel(title = "Output",
        shiny::dataTableOutput("longDataTable")
      ),
      tabPanel(title = "Summary"
      ),
      tabPanel(title = "Plots",
        div(class = "center-button",
            downloadButton("downloadPlot", "Download all fit plots as zip", class = "center-button")
        )
      )
    ),
    tags$script(src="plotter.js")
  )
))

