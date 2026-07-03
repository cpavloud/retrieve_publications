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
.packages = c("httr","jsonlite","rcrossref", "dplyr", "purrr", "stringr", "tidyverse")

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
packageVersion("tidyverse")

################################################################################
############################# Load  files ######################################
################################################################################

#if you have used publish or perish, you should have saved the outputs
#of the searches in .csv files, which you will need to load now

coordinating_projects_papers <- read.csv("coordinating_projects_pubs.csv", sep = ",", header=TRUE) 
EMBRC_HQ_papers <- read.csv("EMBRC_HQ_pubs.csv", sep = ",", header=TRUE) 
EMBRC_nodes_papers <- read.csv("EMBRC_nodes_pubs.csv", sep = ",", header=TRUE) 
EMOBON_papers <- read.csv("EMOBON_pubs.csv", sep = ",", header=TRUE) 
TA_projects_papers <- read.csv("TA_projects_pubs.csv", sep = ",", header=TRUE)

#merge the results
pubs <- rbind(EMOBON_papers, coordinating_projects_papers, TA_projects_papers, 
              EMBRC_HQ_papers, EMBRC_nodes_papers)

#rename columns
pubs <- rename(pubs, title = Title)

# replace blank with NA
pubs[pubs == ""] <- NA

################################################################################
################################################################################
################################################################################

EMOBON_papers <- EMOBON_papers %>% mutate(EMOBON_publication = 'Yes')
EMOBON_pubs <- select(EMOBON_papers, Title, EMOBON_publication)
#rename columns
EMOBON_pubs <- rename(EMOBON_pubs, title = Title)

################################################################################
################################################################################
################################################################################

# add the EMOBON_publication column in the pubs table
pubs <- left_join(pubs, EMOBON_pubs)

for (i in 1:nrow(pubs)) {
  if (is.na(pubs$EMOBON_publication[i]==TRUE)) {
    pubs$EMOBON_publication[i] <-'No'
  }
}

#create doi link column 
#and populate it 
pubs <- pubs %>% mutate(doi_link = NA)
#and populate it 
for (i in 1:nrow(pubs))
  if (!is.na(pubs$DOI[i])){
    pubs$doi_link[i] <- paste('https://doi.org/',pubs$DOI[i])
  }
#delete the space that is introduced by the pasting
pubs$doi_link <- gsub(" ", "", pubs$doi_link)

#select only the unique publications (delete duplicates)
pubs <- unique(pubs)

#create a new column and populate it
pubs <- pubs %>% mutate(any_kind_of_url = NA)
for (i in 1:nrow(pubs))
  if (!is.na(pubs$doi_link[i])){
    pubs$any_kind_of_url[i] <- pubs$doi_link[i]
  } else if (!is.na(pubs$ArticleURL[i])) {
    pubs$any_kind_of_url[i] <- pubs$ArticleURL[i]
  }
pubs <- select(pubs, -ArticleURL)

#delete some rows that are not true publications
pubs <- pubs %>% filter(!str_detect(Type, "CITATION"))

#select just the most important columns for now
pubs <- select(pubs, title, EMOBON_publication, any_kind_of_url)

#select only the unique publications (delete duplicates)
pubs <- unique(pubs)

#delete some rows that are not true publications
pubs <- pubs %>% filter(!str_detect(title, "Deliverable"))

#create an empty data frame
doi_empty <- data.frame() 

#search for publication metadata
for (i in 1:nrow(pubs)) {
  doi <- cr_works(query = pubs$title[i], limit = 5, .progress="text", filter=c(from_pub_date='2024-01-01')) 
  doi <- as.data.frame(doi$data)
  doi_empty <- bind_rows(doi_empty, doi)
}
#select just the unique entries
doi_empty <- unique(doi_empty)

#select certain columns of interest
doi_empty <- select(doi_empty, doi, title, container.title,issued,url, abstract, author, 
                    volume, issue, page, funder)
#rename columns
doi_empty <- rename(doi_empty, journal = container.title)
doi_empty <- rename(doi_empty, date = issued)
#separate the date column
doi_empty <- separate_wider_delim(doi_empty, cols = date, delim = "-", names = c("date", NA, NA), too_few = "align_start")

#correct characters in the data frame
doi_empty$title <- gsub('<i>', "", doi_empty$title)
doi_empty$title <- gsub('</i>', "", doi_empty$title)
doi_empty$title <- gsub('<scp>', "", doi_empty$title)
doi_empty$title <- gsub('</scp>', "", doi_empty$title)
doi_empty$title <- gsub('<sub>', "", doi_empty$title)
doi_empty$title <- gsub('</sub>', "", doi_empty$title)
doi_empty$title <- gsub('<sup>', "", doi_empty$title)
doi_empty$title <- gsub('</sup>', "", doi_empty$title)
doi_empty$title <- gsub('<i>', "", doi_empty$title)
doi_empty$title <- gsub('</i>', "", doi_empty$title)
doi_empty$title <- gsub('&amp; ', "", doi_empty$title)
doi_empty$journal <- gsub('&amp; ', "", doi_empty$journal)
doi_empty$title <- gsub('&lt;em&gt;', "", doi_empty$title)
doi_empty$title <- gsub('&lt;/em&gt;', "", doi_empty$title)
doi_empty$title <- gsub('&lt;i&gt;', "", doi_empty$title)
doi_empty$title <- gsub('&lt;/i&gt;', "", doi_empty$title)
doi_empty$title <- gsub('&amp;#160;', "", doi_empty$title)
doi_empty$title <- gsub('&amp;#8217;s', "'s", doi_empty$title)
doi_empty$journal <- gsub('ACS ES&amp;T', "ACS ES&T", doi_empty$journal)
doi_empty$title <- gsub('&lt;p&gt;', "", doi_empty$title)
doi_empty$title <- gsub('&lt;/p&gt;', "", doi_empty$title)

#delete some rows that are not true publications
doi_empty <- doi_empty %>% filter(!str_detect(url, "/decision"))
doi_empty <- doi_empty %>% filter(!str_detect(url, "/review"))
doi_empty <- doi_empty %>% filter(!str_detect(url, "supplement"))
doi_empty <- doi_empty %>% filter(!str_detect(title, "Figure "))
doi_empty <- doi_empty %>% filter(!str_detect(title, "Deliverable"))
doi_empty <- doi_empty %>% filter(!str_detect(title, "deliverable"))
doi_empty <- doi_empty %>% filter(!str_detect(url, "suppl"))
doi_empty <- doi_empty %>% filter(!str_detect(title, "Supplemental Material"))
doi_empty <- doi_empty %>% filter(!str_detect(title, "Peer Review Report "))
doi_empty <- unique(doi_empty)

#add journal information
doi_empty <- doi_empty %>%
  mutate(journal_2 = case_when(grepl("elife.", url) ~ "eLife", 
                          grepl("f1000research.", url) ~ "F1000Research",
                          grepl("ssrn.", url) ~ "SSRN", 
                          grepl("acmi.", url) ~ "Access Microbiology", 
                          grepl("https://doi.org/10.1101/", url) ~ "bioRxiv", 
                          grepl("https://doi.org/10.18174/", url) ~ "Wageningen University & Research", 
                          grepl("/preprints", url) ~ "Preprints", 
                          grepl("/rs.3.rs", url) ~ "Research Square", 
                          grepl("/oos20", url) ~ "One Ocean Science Congress", 
                          grepl("/arphapreprints", url) ~ "ARPHA Preprints", 
                          grepl("/egusphere", url) ~ "EGU General Assembly", 
                          grepl("/essd", url) ~ "Earth System Science Data", 
                          grepl("/wbf2026", url) ~ "World Biodiversity Forum 2026", 
                          grepl("/cdrxiv", url) ~ "CDRXIV", 
                          grepl("/chemrxiv", url) ~ "ChemRxiv", 
                          grepl("/osf.io", url) ~ "Open Science Framework", 
                          grepl("/au.", url) ~ "Authorea"
                          ))
for (i in 1:nrow(doi_empty)) {
  if (is.na(doi_empty$journal_2[i]==TRUE)) {
    doi_empty$journal_2[i] <-doi_empty$journal[i]  
  }
}
#delete colum
doi_empty <- select(doi_empty, -journal)
#rename columns
doi_empty <- rename(doi_empty, journal = journal_2)

#delete entries if there is no information in the journal column
doi_empty <- doi_empty %>% drop_na(journal)

#delete some columns from the pubs table to merge the tables
pubs <- select(pubs, -any_kind_of_url)
#merge the tables
pubs <- full_join(doi_empty, pubs)

#delete entries if there is no information in the EMOBON_publication column
pubs <- pubs %>% drop_na(EMOBON_publication)

#create the table for the website
website <- select(pubs, title,	date,	journal,	url,	EMOBON_publication)

#create a shorter version of the pubs table for saving
pubs_short <- select(pubs, -author, -funder)


################################################################################
############################## SAVE RESULTS ####################################
################################################################################

write.table(website, "website.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(pubs_short, "pubs_short.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

################################################################################
################################################################################
################################################################################

save.image("Create_file_for_website_perish.RData") # creating ".RData" in current working directory

#Now you have everything in your computer, 
#and you can load it anytime you want by running
#load("Create_file_for_website_perish.RData")

