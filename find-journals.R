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


disc_order <- pub_df %>% 
  group_by(disc) %>% 
  summarise(n = length(disc)) %>% 
  arrange(n) %>% 
  pull(disc)

pub_df %>% 
  group_by(disc, journal.y) %>% 
  summarise(n = length(disc)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(disc = factor(disc, levels =  disc_order)) %>% 
  ggplot(aes(x = disc, y = n, fill = journal.y)) +
  geom_col() +
  scale_x_discrete("Dyscyplina") +
  scale_y_continuous("Liczba publikacji") +
  coord_flip() +
  theme_bw(base_size = 11)
