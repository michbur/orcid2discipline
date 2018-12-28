library(shiny)
library(DT)
library(scholar)
library(dplyr)
library(plotly)
library(stringdist)

httr::set_config(httr::config(http_version = 0))

journal_df <- read.csv("./data/journal-disc.csv")

shinyServer(function(input, output) {
  
  pub_df <- reactive({
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
  
  plot_df <- reactive({
    wrongly_annotated <- if(!is.null(input[["pub-table_rows_selected"]])) {
      pub_table_r()[input[["pub-table_rows_selected"]], "journal_db", drop = TRUE]
    } else {
      NULL
    }
    
    disc_order <- pub_df() %>%
      filter(!(journal_db %in% wrongly_annotated)) %>% 
      group_by(disc) %>% 
      summarise(n = length(disc)) %>% 
      arrange(n) %>% 
      pull(disc)
    
    pub_df() %>% 
      filter(!(journal_db %in% wrongly_annotated)) %>% 
      group_by(disc, journal_list) %>% 
      summarise(n = length(disc)) %>% 
      ungroup() %>% 
      na.omit() %>% 
      mutate(disc = factor(disc, levels =  disc_order))
  })
  
  output[["pub-plot"]] <- renderPlotly({
    p <- if(input[["show_journals"]]) {
      plot_df()  %>% 
        ggplot(aes(x = disc, y = n, fill = journal_list)) +
        geom_col() +
        scale_x_discrete("Dyscyplina") +
        scale_y_continuous("Liczba publikacji") +
        coord_flip() +
        theme_bw(base_size = 15) +
        theme(legend.position = "none")
    } else {
      plot_df()  %>% 
        group_by(disc) %>% 
        summarise(n = sum(n)) %>% 
        ggplot(aes(x = disc, y = n)) +
        geom_col() +
        scale_x_discrete("Dyscyplina") +
        scale_y_continuous("Liczba publikacji") +
        coord_flip() +
        theme_bw(base_size = 15) 
    }
    
    ggplotly(p)
  })
  
  pub_table_r <- reactive({
    pub_df() %>% 
      select(title, journal_db, journal_list, distance) %>% 
      unique() %>% 
      arrange(desc(distance))
  })
  
  output[["pub-table"]] <- DT::renderDataTable({
     datatable(pub_table_r())
  })
  
  output[["pub-plot-panel"]] <- renderUI(
    plotlyOutput("pub-plot", height = paste0(400 + length(unique(pub_df()[["disc"]])) * 30, "px"))
  )
  
})
