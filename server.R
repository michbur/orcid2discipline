library(shiny)
library(DT)
library(scholar)
library(dplyr)
library(plotly)
library(stringdist)

httr::set_config(httr::config(http_version = 0))

journal_df <- read.csv("./data/journal-disc.csv")

shinyServer(function(input, output, session) {
  
  
  pub_raw_df <- reactive({
    scholar_dat <- if(input[["scholar_id"]] == "riuFKDkAAAAJ") {
      load("./data/michal_dat.RData")
      michal_dat
    } else {
      get_publications(input[["scholar_id"]], pagesize = 100)
    }
    
    #if(input[["scholar_id"]] != "riuFKDkAAAAJ")
    #  browser()
    
    scholar_df <- scholar_dat %>% 
      select(title, journal, year) %>%
      unique() %>% 
      filter(journal != "") %>%
      group_by(title, year) %>% 
      summarise(journal = first(journal)) %>%
      ungroup() %>% 
      droplevels() %>% 
      rename(journal_db = journal)
    
    journal_vec <- unique(scholar_df[["journal_db"]])
    journal_all_vec <- levels(journal_df[["journal"]])
    
    all_distances <- stringdistmatrix(a = tolower(journal_vec), b = tolower(journal_all_vec),
                                      method = "jaccard", q = 5)
    
    data.frame(journal_db = journal_vec, 
               journal_list = journal_all_vec[apply(all_distances, 1, which.min)],
               distance = apply(all_distances, 1, min)) %>% 
      inner_join(scholar_df, ., by = c("journal_db" = "journal_db")) %>% 
      inner_join(journal_df, by = c("journal_list" = "journal"))
  })
  
  pub_df <- reactive({
    wrongly_annotated <- if(!is.null(input[["pub-table_rows_selected"]])) {
      pub_table_r()[input[["pub-table_rows_selected"]], "journal_db", drop = TRUE]
    } else {
      NULL
    }
    
    filter(pub_raw_df(), !(journal_db %in% wrongly_annotated))
  })
  
  # Przypisanie czasopism ----------------------------------------
  
  pub_table_r <- reactive({
    pub_raw_df() %>% 
      select(title, journal_db, journal_list, distance) %>% 
      unique() %>% 
      arrange(desc(distance))
  })
  
  output[["pub-table"]] <- DT::renderDataTable(
    datatable(pub_table_r(), style = "bootstrap", filter = "top", 
              extensions = "Buttons",
              options = list(pageLength = 50, dom = "Brtip",
                             buttons = c("copy", "csv", "excel", "print")))
  )
  
  
  # czasopisma i dyscypliny ---------------------------------------------
  
  plot_df <- reactive({
    disc_order <- pub_df() %>% 
      group_by(disc) %>% 
      summarise(n = length(disc)) %>% 
      arrange(n) %>% 
      pull(disc)
    
    pub_df() %>% 
      mutate(disc = factor(disc, levels =  disc_order),
             year = factor(year, levels = sort(unique(year), decreasing = TRUE)))
  })
  
  output[["pub-plot"]] <- renderPlotly({
    
    main_layer <- if(input[["show_journals"]]) {
      geom_bar(aes(x = disc, fill = journal_list))
    } else {
      geom_bar(aes(x = disc))
    }
    
    p <- ggplot(plot_df()) +
      main_layer +
      scale_x_discrete("Dyscyplina") +
      scale_y_continuous("Liczba publikacji") +
      coord_flip() +
      theme_bw(base_size = 15) +
      theme(legend.position = "none")

    ggplotly(p)
  })
  
  output[["pub-plot-panel"]] <- renderUI({
    plot_height <- 400 + length(unique(pub_df()[["disc"]])) * 30 
    plotlyOutput("pub-plot", height = paste0(plot_height, "px"))
  })
  
  # dyscyplina na rok ------------------
  
  output[["disc-plot"]] <- renderPlot({
    filter(pub_df(), disc %in% input[["disc-select"]]) %>% 
      ggplot(aes(x = year)) +
      geom_bar() +
      scale_x_continuous("Rok") +
      scale_y_continuous("Liczba publikacji") +
      theme_bw() +
      facet_wrap(~ disc, ncol = 2) +
      theme_bw(base_size = 15)
    
  })
  
  output[["disc-checkbox"]] <- renderUI({
    
    disc_checbox <- list(tags[["div"]](align = 'left', 
                                       class = 'multicol', 
                                       checkboxGroupInput("disc-select", "Wybierz dyscypliny",
                                                          choices = c(" archeologia", " architektura i urbanistyka", " astronomia", 
                                                                      " automatyka, elektronika i elektrotechnika", " ekonomia i finanse", 
                                                                      " filozofia", " geografia społeczno-ekonomiczna i gospodarka przestrzenna", 
                                                                      " historia", " informatyka ", " informatyka techniczna i telekomunikacja", 
                                                                      " inżynieria biomedyczna", " inżynieria chemiczna", " inżynieria lądowa i transport", 
                                                                      " inżynieria materiałowa", " inżynieria mechaniczna", " inżynieria środowiska, górnictwo i energetyka", 
                                                                      " językoznawstwo", " literaturoznawstwo", " matematyka", " nauki biologiczne", 
                                                                      " nauki chemiczne", " nauki farmaceutyczne", " nauki fizyczne", 
                                                                      " nauki leśne", " nauki medyczne", " nauki o bezpieczeństwie", 
                                                                      " nauki o komunikacji społecznej i mediach", " nauki o kulturze fizycznej", 
                                                                      " nauki o kulturze i religii", " nauki o polityce i administracji", 
                                                                      " nauki o sztuce", " nauki o zarządzaniu i jakości", " nauki o zdrowiu", 
                                                                      " nauki o Ziemi i środowisku", " nauki prawne", " nauki socjologiczne", 
                                                                      " nauki teologiczne", " pedagogika", " prawo kanoniczne", " psychologia", 
                                                                      " rolnictwo i ogrodnictwo", " technologia żywności i żywienia", 
                                                                      " weterynaria", " zootechnika i rybactwo"),
                                                          selected = " nauki socjologiczne")
    ))
    
    
    
    fluidRow(column(width = 12, disc_checbox))
  })
  
  output[["disc-plot-panel"]] <- renderUI({
    plotOutput("disc-plot", height = 400*ceiling(length(input[["disc-select"]])/2))
  })
  
  
})
