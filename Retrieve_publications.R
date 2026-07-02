# ============================================================
'R code for Bibliographic analysis 

Christina Pavloudi
christina.pavloudi@embrc.eu
https://cpavloud.github.io/mysite/

	Copyright (C) 2026 Christina Pavloudi
  
    This script is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
  
    This script is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.'

# =============================================================


################################################################################
############################ LOAD LIBRARIES ####################################
################################################################################

# List of packages needed
.packages = c("httr","jsonlite","rcrossref", "dplyr", "purrr", "stringr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

packageVersion("httr")
packageVersion("jsonlite")
packageVersion("rcrossref")
packageVersion("dplyr")
packageVersion("purrr")
packageVersion("stringr")

################################################################################
######################### BUILD THE FUNCTION ###################################
################################################################################


search_combined_literature <- function(
    queries,
    start_year = 2020,
    end_year = 2024,
    limit_per_query = 20,
    sleep_time = 1
) {
  
  # -------------------------
  # Semantic Scholar fetch
  # -------------------------
  fetch_semantic <- function(query) {
    url <- paste0(
      "https://api.semanticscholar.org/graph/v1/paper/search?",
      "query=", URLencode(query),
      "&limit=", limit_per_query,
      "&fields=title,year,authors,url"
    )
    
    res <- GET(url)
    Sys.sleep(sleep_time)
    
    if (status_code(res) != 200) return(NULL)
    
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (is.null(data$data)) return(NULL)
    
    df <- data$data %>%
      mutate(
        authors = map_chr(authors, ~paste(.x$name, collapse = ", ")),
        doi = NA,
        source = "SemanticScholar",
        query = query
      ) %>%
      select(title, year, authors, doi, url, source, query)
    
    return(df)
  }
  
  # -------------------------
  # CrossRef fetch
  # -------------------------
  fetch_crossref <- function(query) {
    res <- tryCatch({
      cr_works(
        query = query,
        limit = limit_per_query,
        filter = c(
          from_pub_date = paste0(start_year, "-01-01"),
          until_pub_date = paste0(end_year, "-12-31")
        )
      )
    }, error = function(e) return(NULL))
    
    if (is.null(res) || nrow(res$data) == 0) return(NULL)
    
    df <- res$data %>%
      transmute(
        title = title,
        year = as.numeric(substr(issued, 1, 4)),
        authors = map_chr(author, ~{
          if (is.null(.x)) return(NA)
          paste(.x$family, collapse = ", ")
        }),
        doi = doi,
        url = url,
        source = "CrossRef",
        query = query
      )
    
    return(df)
  }
  
  # -------------------------
  # Run queries
  # -------------------------
  semantic_res <- map(queries, fetch_semantic) %>% bind_rows()
  crossref_res <- map(queries, fetch_crossref) %>% bind_rows()
  
  combined <- bind_rows(semantic_res, crossref_res)
  
  if (nrow(combined) == 0) {
    message("No results found.")
    return(NULL)
  }
  
  # -------------------------
  # Filter by year
  # -------------------------
  combined <- combined %>%
    filter(!is.na(year) & year >= start_year & year <= end_year)
  
  # -------------------------
  # Deduplicate (by title)
  # -------------------------
  combined <- combined %>%
    mutate(title_clean = str_to_lower(title)) %>%
    distinct(title_clean, .keep_all = TRUE) %>%
    select(-title_clean)
  
  # -------------------------
  # Sort by most recent
  # -------------------------
  combined <- combined %>%
    arrange(desc(year))
  
  return(combined)
}


################################################################################
############################## KEYWORD QUERIES #################################
################################################################################

#specify the keywords for the search
EMOBON_query <- c(
  "ΕΜΟ ΒΟΝ",
  "EMOBON",
  "EMO-BON"
)

coordinating_projects_query <- c(
  "101112800", 
  "eDNAquaPlan","eDNAqua-plan",
  "eDNAqua-Plan",
  "730984",
  "assemble plus",
  "ASSEMBLE Plus",
  "101082021",
  "MARCO-BOLO"
)

TA_projects_query <- c(
  "227799", "assemble marine", "654008", "EMBRIC" ,"730984" , "assemble plus" ,"ASSEMBLE Plus" ,
  "101131121" , "AQUASERV" , "101058620", "CanServ" , "CANSERV" , "101058020" , "AgroServ" , 
  "AGROSERV" , "101131261" , "IRISCC", "101130915"
)

EMBRC_HQ_query <- c("EMBRC" , "EMBRC-ERIC")
  
EMBRC_nodes_query <- c("EMBRC-BE" , "EMBRC-Belgium" , "EMBRC Belgium" , "EMBRC-ES" , "EMBRC-Spain" , 
                       "EMBRC Spain" , "EMBRC-PT" , "EMBRC-Portugal" , "EMBRC.PT" , "EMBRC Portugal" , 
                       "ALG-01-0145-FEDER-022121" , "FEDER022121", "EMBRC-GR" , "EMBRC-Greece" , "CMBR",
                       "EMBRC Greece" , "EMBRC Sweden" , "EMBRC-SE" , "EMBRC-Sweden", "EMBRC-FR" ,
                       "EMBRC-France" , "EMBRC France" , "EMBRC Italy" , "EMBRC-IT" , "EMBRC-Italy" ,
                       "EMBRC Norway" , "EMBRC-NO" , "EMBRC-Norway","EMBRC-FI" , "EMBRC-Finland" , 
                       "EMBRC Finland" , "EMBRC Israel" , "EMBRC-IL" , "EMBRC-Israel" , 
                       "EMBRC United Kingdom" , "EMBRC-UK" , "EMBRC-United Kingdom" , "EMBRC UK"
)

################################################################################
############################### RETRIEVAL ######################################
################################################################################
  
#run the function to retrieve publications for the selected keywords
#remember to specify the start and end year
EMOBON_papers <- search_combined_literature(
  EMOBON_query,
  start_year = 2025,
  end_year = 2025,
  limit_per_query = 50
)

#filter results to delete erroneous rows
EMOBON_papers <- EMOBON_papers %>%
  mutate(keep = case_when(grepl("EMO BON", title) ~ "Yes", 
                          grepl("EMOBON", title) ~ "Yes",
                          grepl("EMO-BON", title) ~ "Yes"))
for (i in 1:nrow(EMOBON_papers)) {
  if (is.na(EMOBON_papers$keep[i]==TRUE)) {
    EMOBON_papers$keep[i] <-'No'
  }
}
EMOBON_papers <- EMOBON_papers %>% filter(str_detect(keep, "Yes"))
EMOBON_papers <- select(EMOBON_papers, -keep)

#correct the query column
EMOBON_papers$query <- gsub("EMOBON", "EMO BON", EMOBON_papers$query)
EMOBON_papers$query <- gsub("EMO-BON", "EMO BON", EMOBON_papers$query)

coordinating_projects_papers <- search_combined_literature(
  coordinating_projects_query,
  start_year = 2025,
  end_year = 2025,
  limit_per_query = 50
)
#filter results to delete erroneous rows
coordinating_projects_papers <- coordinating_projects_papers %>%
  mutate(keep = case_when(grepl("plan.", doi) ~ "No", 
                          grepl("eDNAqua-Plan", title) ~ "Yes", 
                          grepl("MARCO-BOLO", title) ~ "Yes", 
                          grepl("ASSEMBLE Plus", title) ~ "Yes", 
                          grepl("730984", query) ~ "Yes",
                          grepl("101082021", query) ~ "Yes",
                          grepl("101112800", query) ~ "Yes"))
for (i in 1:nrow(coordinating_projects_papers)) {
  if (is.na(coordinating_projects_papers$keep[i]==TRUE)) {
    coordinating_projects_papers$keep[i] <-'No'
  }
}
coordinating_projects_papers <- coordinating_projects_papers %>% filter(str_detect(keep, "Yes"))
coordinating_projects_papers <- select(coordinating_projects_papers, -keep)


#correct the query column
coordinating_projects_papers$query <- gsub("730984", "ASSEMBLE Plus", coordinating_projects_papers$query)
coordinating_projects_papers$query <- gsub("assemble plus", "ASSEMBLE Plus", coordinating_projects_papers$query)
coordinating_projects_papers$query <- gsub("101112800", "eDNAqua-Plan", coordinating_projects_papers$query)
coordinating_projects_papers$query <- gsub("eDNAquaPlan", "eDNAqua-Plan", coordinating_projects_papers$query)
coordinating_projects_papers$query <- gsub("eDNAqua-plan", "eDNAqua-Plan", coordinating_projects_papers$query)
coordinating_projects_papers$query <- gsub("101082021", "MARCO-BOLO", coordinating_projects_papers$query)


TA_projects_papers <- search_combined_literature(
  TA_projects_query,
  start_year = 2025,
  end_year = 2025,
  limit_per_query = 50
)

#filter results to delete erroneous rows
TA_projects_papers <- TA_projects_papers %>%
  mutate(keep = case_when(grepl("EMBRIC", title) ~ "Yes", 
                          grepl("AQUASERV", title) ~ "Yes", 
                          grepl("canSERV", title) ~ "Yes", 
                          grepl("AgroServ", title) ~ "Yes", 
                          grepl("ASSEMBLE Plus", title) ~ "Yes", 
                          grepl("IRISCC", title) ~ "Yes", 
                          grepl("730984", query) ~ "Yes",
                          grepl("101130915", query) ~ "Yes",
                          grepl("227799", query) ~ "Yes", 
                          grepl("654008", query) ~ "Yes",
                          grepl("101131121", query) ~ "Yes",
                          grepl("101058620", query) ~ "Yes",
                          grepl("101058020", query) ~ "Yes",
                          grepl("101131261", query) ~ "Yes"))
for (i in 1:nrow(TA_projects_papers)) {
  if (is.na(TA_projects_papers$keep[i]==TRUE)) {
    TA_projects_papers$keep[i] <-'No'
  }
}
TA_projects_papers <- TA_projects_papers %>% filter(str_detect(keep, "Yes"))
TA_projects_papers <- select(TA_projects_papers, -keep)

#correct the query column
TA_projects_papers$query <- gsub("730984", "ASSEMBLE Plus", TA_projects_papers$query)
TA_projects_papers$query <- gsub("assemble plus", "ASSEMBLE Plus", TA_projects_papers$query)
TA_projects_papers$query <- gsub("101130915", "AQUARIUS", TA_projects_papers$query)
TA_projects_papers$query <- gsub("101131121", "AQUASERV", TA_projects_papers$query)
TA_projects_papers$query <- gsub("101058620", "canSERV", TA_projects_papers$query)
TA_projects_papers$query <- gsub("CanServ", "canSERV", TA_projects_papers$query)
TA_projects_papers$query <- gsub("CANSERV", "canSERV", TA_projects_papers$query)
TA_projects_papers$query <- gsub("101058020", "AGROSERV", TA_projects_papers$query)
TA_projects_papers$query <- gsub("AgroServ", "AGROSERV", TA_projects_papers$query)
TA_projects_papers$query <- gsub("227799", "ASSEMBLE", TA_projects_papers$query)
TA_projects_papers$query <- gsub("654008", "EMBRIC", TA_projects_papers$query)
TA_projects_papers$query <- gsub("101131261", "IRISCC", TA_projects_papers$query)


EMBRC_HQ_papers <- search_combined_literature(
  EMBRC_HQ_query,
  start_year = 2025,
  end_year = 2025,
  limit_per_query = 50
)

#filter results to delete erroneous rows
EMBRC_HQ_papers <- EMBRC_HQ_papers %>%
  mutate(keep = case_when(query == "EMBRC" ~ "Yes", 
                          grepl("EMBRC", title) ~ "Yes",
                          grepl("EMBRC-ERIC", title) ~ "Yes"))
for (i in 1:nrow(EMBRC_HQ_papers)) {
  if (is.na(EMBRC_HQ_papers$keep[i]==TRUE)) {
    EMBRC_HQ_papers$keep[i] <-'No'
  }
}
EMBRC_HQ_papers <- EMBRC_HQ_papers %>% filter(str_detect(keep, "Yes"))
EMBRC_HQ_papers <- select(EMBRC_HQ_papers, -keep)


EMBRC_nodes_papers <- search_combined_literature(
  EMBRC_nodes_query,
  start_year = 2025,
  end_year = 2025,
  limit_per_query = 50
)

#filter results to delete erroneous rows
EMBRC_nodes_papers <- EMBRC_nodes_papers %>%
  mutate(keep = case_when(grepl("ALG-01-0145-FEDER-022121", query) ~ "Yes", 
                          grepl("FEDER022121", title) ~ "Yes",
                          grepl("EMBRC-BE", title) ~ "Yes",
                          grepl("EMBRC-BE", query) ~ "Yes",
                          grepl("EMBRC Belgium", title) ~ "Yes",
                          grepl("EMBRC-Belgium", title) ~ "Yes",
                          grepl("BE", title) & query == "EMBRC-BE" ~ "No",
                          grepl("Belgium", title) & grepl("Belgium", query) ~ "No",
                          grepl("EMBRC-ES", title) ~ "Yes",
                          grepl("EMBRC Spain", title) ~ "Yes",
                          grepl("EMBRC-Spain", title) ~ "Yes",
                          grepl("ES", title) & query == "EMBRC-ES" ~ "No",
                          grepl("Spain", title) & grepl("Spain", query) ~ "No",
                          grepl("EMBRC-PT", title) ~ "Yes",
                          grepl("EMBRC Portugal", title) ~ "Yes",
                          grepl("EMBRC-Portugal", title) ~ "Yes",
                          grepl("EMBRC.PT", title) ~ "Yes",
                          grepl("PT", title) & query == "EMBRC-PT" ~ "No",
                          grepl("Portugal", title) & grepl("Portugal", query) ~ "No",
                          grepl("EMBRC-GR", title) ~ "Yes",
                          grepl("EMBRC Greece", title) ~ "Yes",
                          grepl("EMBRC-Greece", title) ~ "Yes",
                          grepl("CMBR", title) ~ "Yes",
                          grepl("GR", title) & query == "EMBRC-GR" ~ "No",
                          grepl("Greece", title) & grepl("Greece", query) ~ "No",
                          grepl("EMBRC-SE", title) ~ "Yes",
                          grepl("EMBRC Sweden", title) ~ "Yes",
                          grepl("EMBRC-Sweden", title) ~ "Yes",
                          grepl("SE", title) & query == "EMBRC-SE" ~ "No",
                          grepl("Sweden", title) & grepl("Sweden", query) ~ "No",
                          grepl("EMBRC-FR", title) ~ "Yes",
                          grepl("EMBRC France", title) ~ "Yes",
                          grepl("EMBRC-France", title) ~ "Yes",
                          grepl("FR", title) & query == "EMBRC-FR" ~ "No",
                          grepl("France", title) & grepl("France", query) ~ "No",
                          grepl("EMBRC-IT", title) ~ "Yes",
                          grepl("EMBRC Italy", title) ~ "Yes",
                          grepl("EMBRC-Italy", title) ~ "Yes",
                          grepl("IT", title) & query == "EMBRC-IT" ~ "No",
                          grepl("Italy", title) & grepl("Italy", query) ~ "No",
                          grepl("EMBRC-NO", title) ~ "Yes",
                          grepl("EMBRC Norway", title) ~ "Yes",
                          grepl("EMBRC-Norway", title) ~ "Yes",
                          grepl("NO", title) & query == "EMBRC-NO" ~ "No",
                          grepl("Norway", title) & grepl("Norway", query) ~ "No",
                          grepl("EMBRC-FI", title) ~ "Yes",
                          grepl("EMBRC Finland", title) ~ "Yes",
                          grepl("EMBRC-Finland", title) ~ "Yes",
                          grepl("FI", title) & query == "EMBRC-FI" ~ "No",
                          grepl("Finland", title) & grepl("Finland", query) ~ "No",
                          grepl("EMBRC-IL", title) ~ "Yes",
                          grepl("EMBRC Israel", title) ~ "Yes",
                          grepl("EMBRC-Israel", title) ~ "Yes",
                          grepl("IL", title) & query == "EMBRC-IL" ~ "No",
                          grepl("Israel", title) & grepl("Israel", query) ~ "No",
                          grepl("EMBRC-UK", title) ~ "Yes",
                          grepl("EMBRC United Kingdom", title) ~ "Yes",
                          grepl("EMBRC-United Kingdom", title) ~ "Yes",
                          grepl("EMBRC UK", title) ~ "Yes",
                          grepl("UK", title) & query == "EMBRC-UK" ~ "No",
                          grepl("United Kingdom", title) & grepl("United Kingdom", query) ~ "No",
                          grepl("United", title) & grepl("United Kingdom", query) ~ "No"))
for (i in 1:nrow(EMBRC_nodes_papers)) {
  if (is.na(EMBRC_nodes_papers$keep[i]==TRUE)) {
    EMBRC_nodes_papers$keep[i] <-'No'
  }
}
EMBRC_nodes_papers <- EMBRC_nodes_papers %>% filter(str_detect(keep, "Yes"))
EMBRC_nodes_papers <- select(EMBRC_nodes_papers, -keep)

#correct the query column
EMBRC_nodes_papers$query <- gsub("EMBRC-BE", "EMBRC Belgium", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Belgium", "EMBRC Belgium", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-ES", "EMBRC Spain", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Spain", "EMBRC Spain", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-PT", "EMBRC Portugal", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Portugal", "EMBRC Portugal", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC.PT", "EMBRC Portugal", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("ALG-01-0145-FEDER-022121", "EMBRC Portugal", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("FEDER022121", "EMBRC Portugal", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-GR", "EMBRC Greece", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Greece", "EMBRC Greece", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("CMBR", "EMBRC Greece", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-SE", "EMBRC Sweden", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Sweden", "EMBRC Sweden", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-FR", "EMBRC France", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-France", "EMBRC France", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-IT", "EMBRC Italy", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Italy", "EMBRC Italy", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-NO", "EMBRC Norway", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Norway", "EMBRC Norway", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-FI", "EMBRC Finland", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Finland", "EMBRC Finland", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-IL", "EMBRC Israel", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-Israel", "EMBRC Israel", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-UK", "EMBRC United Kingdom", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC-United Kingdom", "EMBRC United Kingdom", EMBRC_nodes_papers$query)
EMBRC_nodes_papers$query <- gsub("EMBRC UK", "EMBRC United Kingdom", EMBRC_nodes_papers$query)


################################################################################
############################## SAVE RESULTS ####################################
################################################################################

write.table(EMOBON_papers, "EMOBON_papers.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(coordinating_projects_papers, "coordinating_projects_papers.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(TA_projects_papers, "TA_projects_papers.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(EMBRC_HQ_papers, "EMBRC_HQ_papers.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(EMBRC_nodes_papers, "EMBRC_nodes_papers.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

################################################################################
################################################################################
################################################################################

save.image("Retrieve_publications.RData") # creating ".RData" in current working directory

					
