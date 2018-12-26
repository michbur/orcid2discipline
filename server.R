library(shiny)
library(DT)
library(scholar)
library(dplyr)
library(plotly)

journal_df <- read.csv("./data/journal-disc.csv")

shinyServer(function(input, output) {
   
  pub_df <- reactive({
    scholar_id <- "riuFKDkAAAAJ"
    
    scholar_dat <- if(scholar_id == "riuFKDkAAAAJ") {
      load("./data/michal_dat.RData")
      michal_dat
    } else {
      get_publications(scholar_id, pagesize = 100)
    }
    
    scholar_df <- scholar_dat %>% 
      select(title, journal) %>%
      unique() %>% 
      filter(journal != "") %>% 
      droplevels() %>% 
      rename(journal_scholar = journal)
    
    journal_vec <- levels(scholar_df[["journal_scholar"]])
    journal_all_vec <- levels(journal_df[["journal"]])
    
    all_distances <- adist(journal_vec, journal_all_vec, ignore.case = TRUE, partial = FALSE,
                             costs = c(insertions = 10, deletions = 4, substitutions = 10))
    
    data.frame(journal_scholar = journal_vec, journal_list = journal_all_vec[apply(all_distances, 1, which.min)]) %>% 
      inner_join(scholar_df, ., by = c("journal_scholar" = "journal_scholar")) %>% 
      inner_join(journal_df, by = c("journal_list" = "journal"))
  })
  
  output[["pub-plot"]] <- renderPlot({
    disc_order <- pub_df() %>% 
      group_by(disc) %>% 
      summarise(n = length(disc)) %>% 
      arrange(n) %>% 
      pull(disc)
    
    p <- pub_df() %>% 
      group_by(disc, journal_list) %>% 
      summarise(n = length(disc)) %>% 
      ungroup() %>% 
      na.omit() %>% 
      mutate(disc = factor(disc, levels =  disc_order)) %>% 
      ggplot(aes(x = disc, y = n, fill = journal_list)) +
      geom_col() +
      scale_x_discrete("Dyscyplina") +
      scale_y_continuous("Liczba publikacji") +
      scale_fill_discrete(guide = FALSE) +
      coord_flip() +
      theme_bw(base_size = 15) 
    
    p
  })
  
  output[["pub-table"]] <- DT::renderDataTable({
    pub_df() %>% 
      select(title, journal_scholar, journal_list) %>% 
      unique() %>% 
      datatable()
  })
  
  output[["pub-plot-panel"]] <- renderUI(
    plotOutput("pub-plot", height = 400 + length(unique(pub_df()[["disc"]])) * 30)
  )
  
})
