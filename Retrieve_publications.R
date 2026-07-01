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

#Build the function that will retrieve the publications
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
      #"&fields=title,abstract,year,authors,url"
    )
    
    res <- GET(url)
    Sys.sleep(sleep_time)
    
    if (status_code(res) != 200) return(NULL)
    
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (is.null(data$data)) return(NULL)
    
    df <- data$data
    
    df <- df %>%
      mutate(
        source = "SemanticScholar",
        query = query,
        doi = NA,
        keywords = NA
      )
    
    return(df)
  }
  
  # -------------------------
  # CrossRef fetch
  # -------------------------
  #clean_abstract <- function(x) {
  #  if (is.null(x) || is.na(x)) return(NA)
  #  str_replace_all(x, "<[^>]+>", "")
  #}
  
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
    
    df <- res$data
    
    df_clean <- df %>%
      transmute(
        title = title,
        #abstract = map_chr(abstract, clean_abstract),
        year = as.numeric(substr(issued, 1, 4)),
        doi = doi,
        url = url,
        type = type,
        source = "CrossRef",
        query = query
      )
    
    return(df_clean)
  }
  
  # -------------------------
  # Run queries
  # -------------------------
  semantic_res <- map(queries, fetch_semantic) %>% bind_rows()
  crossref_res <- map(queries, fetch_crossref) %>% bind_rows()
  
  # Harmonize Semantic schema
  if (nrow(semantic_res) > 0) {
    semantic_res <- semantic_res %>%
      mutate(
        authors = map_chr(authors, ~paste(.x$name, collapse = ", "))
      ) %>%
      select(title, year, authors, doi, url, keywords, source, query)
    #select(title, abstract, year, authors, doi, url, keywords, source, query)
  }
  
  # Merge
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
  # Deduplicate (by title similarity)
  # -------------------------
  combined <- combined %>%
    mutate(title_clean = str_to_lower(title)) %>%
    distinct(title_clean, .keep_all = TRUE) %>%
    select(-title_clean)
  
  # -------------------------
  # Section-aware filtering (workaround)
  # -------------------------
  matches_query <- function(text, query) {
    if (is.na(text)) return(FALSE)
    str_detect(str_to_lower(text), str_to_lower(query))
  }
  
  combined <- combined %>%
    filter(
      map2_lgl(title, query, matches_query)
    )
  
  #combined <- combined %>%
  #  filter(
  #    map2_lgl(title, query, matches_query) |
  #      map2_lgl(abstract, query, matches_query)
  #  )
  
  
  # -------------------------
  # Sort
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
  "EMO BON",
  "EMOBON",
  "EMO-BON"
)

coordinating_projects_query <- c(
  "101112800", 
  "eDNAquaPlan",
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
                       "ALG-01-0145-FEDER-022121" , "FEDER022121", "EMBRC-GR" , "EMBRC-Greece" , 
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
  start_year = 2026,
  end_year = 2026,
  limit_per_query = 50
)

coordinating_projects_papers <- search_combined_literature(
  coordinating_projects_query,
  start_year = 2026,
  end_year = 2026,
  limit_per_query = 50
)

TA_projects_papers <- search_combined_literature(
  TA_projects_query,
  start_year = 2026,
  end_year = 2026,
  limit_per_query = 50
)

EMBRC_HQ_papers <- search_combined_literature(
  EMBRC_HQ_query,
  start_year = 2026,
  end_year = 2026,
  limit_per_query = 50
)

EMBRC_nodes_papers <- search_combined_literature(
  EMBRC_nodes_query,
  start_year = 2026,
  end_year = 2026,
  limit_per_query = 50
)

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
