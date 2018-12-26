library(scholar)
library(rorcid)
library(fuzzyjoin)
library(stringr)
library(dplyr)
library(jsonlite)
library(ggplot2)

journal_df <- read.csv("./data/journal-disc.csv")

scholar_dat <- get_publications("riuFKDkAAAAJ", pagesize = 100)

pub_df <- scholar_dat %>% 
  select(title, journal) %>%
  unique() %>% 
  filter(journal != "") %>% 
  droplevels()

journal_vec <- levels(pub_df[["journal"]])
journal_all_vec <- levels(journal_df[["journal"]])

matched_journal <- lapply(journal_vec, function(ith_pub) {
  all_distances <- adist(journal_vec, journal_all_vec, ignore.case = TRUE, partial = FALSE,
                         costs = c(insertions = 10, deletions = 4, substitutions = 10))
  data.frame(journal = journal_vec, matched = journal_all_vec[apply(all_distances, 1, which.min)])
})


