library(rorcid)
library(rcrossref)

orcid_personal <- orcid_person("0000-0001-8926-582X")

orcid_personal[[1]][["name"]][["given-names"]][["value"]]
orcid_personal[[1]][["name"]][["family-name"]][["value"]]

orcid_institution <- orcid_employments("0000-0001-8926-582X")
orcid_institution[[1]][["employment-summary"]][1, "organization.name"]

orcid_pubs <- orcid_works("0000-0001-8926-582X")


ith_pub <- orcid_pubs[[1]][["group"]][["work-summary"]][[14]]

dois_df <- lapply(orcid_pubs[[1]][["group"]][["work-summary"]], function(ith_pub) {
  doi <- ith_pub[["external-ids.external-id"]] %>% 
    bind_rows() %>% 
    filter(`external-id-type` == "doi") %>% 
    pull(`external-id-value`) %>% 
    unique

  data.frame(orcid_title = ith_pub[1, "title.title.value"],
             doi = ifelse(length(doi) == 0, NA, doi))
}) %>% bind_rows()


cr_df <- cr_works(dois = na.omit(dois_df[["doi"]]))
select(cr_df[["data"]],  container.title, doi, title) %>% 
  left_join(dois_df, by = c("doi" = "doi")) %>% 
  select(title = orcid_title, journal_db = orcid_title)
