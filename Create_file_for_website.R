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

#you can either read the files that you saved on the previous step
EMOBON_papers <- read.csv("EMOBON_papers.tsv", sep = "\t", header=TRUE)
coordinating_projects_papers <- read.csv("coordinating_projects_papers.tsv", sep = "\t", header=TRUE)
TA_projects_papers <- read.csv("TA_projects_papers.tsv", sep = "\t", header=TRUE)
EMBRC_HQ_papers <- read.csv("EMBRC_HQ_papers.tsv", sep = "\t", header=TRUE)
EMBRC_nodes_papers <- read.csv("EMBRC_nodes_papers.tsv", sep = "\t", header=TRUE)

#or you can load the entire workspace
load("Retrieve_publications.RData")

#merge the results
pubs <- rbind(EMOBON_papers, coordinating_projects_papers, TA_projects_papers, 
             EMBRC_HQ_papers, EMBRC_nodes_papers)

################################################################################
################################################################################
################################################################################

#add an extra (yes/no) column in the EMO BON publications 
#specifying that they are considered EMO BON publications
#this will be useful afterwards
EMOBON_papers <- EMOBON_papers %>% mutate(EMOBON_publication = 'Yes')
EMOBON_pubs <- select(EMOBON_papers, title, EMOBON_publication)

################################################################################
################################################################################
################################################################################

#create doi link column 
#and populate it 
pubs <- pubs %>% mutate(doi_link = NA)
#and populate it 
for (i in 1:nrow(pubs))
  if (!is.na(pubs$doi[i])){
    pubs$doi_link[i] <- paste('https://doi.org/',pubs$doi[i])
  }
#delete the space that is introduced by the pasting
pubs$doi_link <- gsub(" ", "", pubs$doi_link)

#select only the unique publications (delete duplicates)
pubs <- unique(pubs)

# replace blank with NA
pubs[pubs == ""] <- NA

#delete entries if there is no information in the doi column
pubs <- pubs %>% drop_na(doi)

#delete the query column
pubs <- select(pubs, -query)

#select only the unique publications (delete duplicates)
pubs <- unique(pubs)

# add the EMOBON_publication column in the pubs table
pubs <- left_join(pubs, EMOBON_pubs)

for (i in 1:nrow(pubs)) {
  if (is.na(pubs$EMOBON_publication[i]==TRUE)) {
    pubs$EMOBON_publication[i] <-'No'
  }
}

#create an empty data frame
doi_empty <- data.frame() 

#search for publication metadata
for (i in 1:nrow(pubs)) {
  doi <- cr_works(dois = pubs$doi[i], .progress="text") 
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
doi_empty <- unique(doi_empty)

#delete some columns from the pubs table to merge the tables
pubs <- select(pubs, -title, -doi_link)
#merge the tables
pubs <- full_join(doi_empty, pubs)

#create the table for the website
website <- select(pubs, title,	date,	journal,	url,	EMOBON_publication)


################################################################################
############################## SAVE RESULTS ####################################
################################################################################

write.table(website, "website.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

################################################################################
################################################################################
################################################################################

save.image("Create_file_for_website.RData") # creating ".RData" in current working directory

#Now you have everything in your computer, 
#and you can load it anytime you want by running
#load("Create_file_for_website.RData")


