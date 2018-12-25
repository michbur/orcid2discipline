library(dplyr)

journals <- readLines("https://raw.githubusercontent.com/pbiecek/SmarterPoland_blog/master/2018/dyscypliny/Wykaz_czasopism_z_baz_Scopus_i_Web_of_Science_-_dyscypliny.txt")

splitted_journals <- split(journals, cumsum(!is.na(as.numeric(journals))))

parse_journal <- function(x) {
  journal_vec <- lapply(split(x, cumsum(x == "")), paste0, collapse = "")
  
  disc_vec <- journal_vec[[grep(";", journal_vec, fixed = TRUE)]] %>% 
    strsplit(";") %>% 
    unlist
  
  data.frame(journal = journal_vec[[2]],
             disc = disc_vec,
             stringsAsFactors = FALSE)
}

journal_df <- lapply(splitted_journals[-c(1, length(splitted_journals))], parse_journal) %>% 
  bind_rows()

write.csv(x = journal_df, file = "./data/journal-disc.csv", row.names = FALSE)
