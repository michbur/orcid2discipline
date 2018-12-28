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
                value = "riuFKDkAAAAJ")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Przypisanie czasopism", 
                 DT::dataTableOutput("pub-table") %>% withSpinner(color="#0dc5c1")
        ),
        #plotOutput("pub-plot", height = 1200)),
        tabPanel("Czasopisma i dyscypliny", 
                 checkboxInput("show_journals",
                               "Pokaż wkład poszczególnych czasopism",
                               value = FALSE),
                 uiOutput("pub-plot-panel") %>% withSpinner(color="#0dc5c1")
        ),
        tabPanel("Publikacje w dyscyplinach",
                 plotlyOutput("disc-plot", height = "900px")
        )
      )
    )
  )
))
