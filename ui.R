library(shiny)
library(DT)
library(shinycssloaders)
library(plotly)

shinyUI(fluidPage(
  
  # source: https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny/33108619#33108619
  list(tags[["head"]](tags[["style"]](HTML("
                                                     .multicol { 
                                                     height: 350px;
                                                     -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                                     -moz-column-count: 3;    /* Firefox */ 
                                                     column-count: 3; 
                                                     -moz-column-fill: auto;
                                                     -column-fill: auto;
                                                     } 
                                                     ")) 
  )),
  
  titlePanel("Publikacje a dyscypliny"),
  fluidRow(column(width = 2, textInput("scholar_id",
                                       "ID w Google Scholar:", 
                                       value = "riuFKDkAAAAJ")),
           column(width = 4, uiOutput("researcher_name"))),
  tabsetPanel(
    tabPanel("Przypisanie czasopism", 
             includeMarkdown("./readmes/readme1.md"),
             DT::dataTableOutput("pub-table") %>% withSpinner(color="#0dc5c1")
    ),
    #plotOutput("pub-plot", height = 1200)),
    tabPanel("Czasopisma i dyscypliny", 
             checkboxInput("show_journals",
                           "Pokaż wkład poszczególnych czasopism",
                           value = FALSE),
             uiOutput("pub-plot-panel") %>% withSpinner(color="#0dc5c1")
    ),
    tabPanel("Publikacje w dyscyplinach (rocznie)",
             uiOutput("disc-checkbox"),
             uiOutput("disc-plot-panel") %>% withSpinner(color="#0dc5c1")
    ),
    tabPanel("O narzędziu",
             includeMarkdown("readme.md"))
  )
  
  
))
