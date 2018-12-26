library(shiny)
library(DT)

shinyUI(fluidPage(
  
  titlePanel("Publikacje a dyscypliny"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("scholar_id",
                "ID w Google Scholar:", 
                value = "riuFKDkAAAAJ"),
      checkboxInput("show_journals",
                    "Pokaż wkład poszczególnych czasopism do dyscyplin",
                    value = FALSE)
    ),
    
    mainPanel(
      DT::dataTableOutput("pub-table"),
      #plotOutput("pub-plot", height = 1200)),
      uiOutput("pub-plot-panel")
    )
  )
))
