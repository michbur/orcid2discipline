library(shiny)
library(DT)
library(shinycssloaders)
library(plotly)

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
      DT::dataTableOutput("pub-table") %>% 
        withSpinner(color="#0dc5c1"),
      #plotOutput("pub-plot", height = 1200)),
      uiOutput("pub-plot-panel")
    )
  )
))
