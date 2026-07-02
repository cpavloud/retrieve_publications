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

#if you have used publish or perish, you should load the workspace
load("Create_file_for_website_perish.RData")

#if you have used the Retrieve_publications.R script, you should load the workspace
load("Create_file_for_website.RData")

################################################################################
################################################################################
################################################################################

#unnest the author column
pubs_authors <- pubs %>%
  mutate(author = map(author, ~ replace(., is.null(.), NA))) %>% 
  unnest(c(author))  

#delete column that creates confusion
pubs_authors <- select(pubs_authors, -name)

#unnest the funder column
pubs_authors_funder <- pubs_authors %>%
  mutate(funder = map(funder, ~ replace(., is.null(.), NA))) %>% 
  unnest(c(funder))  

#rename columns
pubs_authors <- rename(pubs_authors, name = given)
pubs_authors <- rename(pubs_authors, surname = family)

#load the function that searches through the data table
find_text_filter <- function(df, tt, ...){
  res <- df %>%
    filter(if_any(where(is.character), ~grepl(tt,.x, ...)))
  return(res)
}

################################################################################
############################### SWEDEN #########################################
################################################################################

Sweden_1 <- pubs_authors_funder %>% find_text_filter("Linnaeus University") 
Sweden_2 <- pubs_authors_funder %>% find_text_filter("Swedish University of Agricultural Sciences") 
Sweden_3 <- pubs_authors_funder %>% find_text_filter("Swedish Meteorological and Hydrological Institute") 
Sweden_4 <- pubs_authors_funder %>% find_text_filter("Stockholm University") 
Sweden_5 <- pubs_authors_funder %>% find_text_filter("Umeå University") 
Sweden_6 <- pubs_authors_funder %>% find_text_filter("Umea University") 
Sweden_7 <- pubs_authors_funder %>% find_text_filter("Uppsala University") 
Sweden_8 <- pubs_authors_funder %>% find_text_filter("University of Gothenburg") 
Sweden <- rbind(Sweden_1, Sweden_2, Sweden_3, Sweden_4, Sweden_5, Sweden_6, Sweden_7, Sweden_8)
Sweden <- select(Sweden, doi)
Sweden <- unique(Sweden)

Sweden <- semi_join(pubs_authors, Sweden)
Sweden <- select(Sweden, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                 date, EMOBON_publication, abstract)


################################################################################
################################ SPAIN #########################################
################################################################################

Spain_1 <- pubs_authors_funder %>% find_text_filter("University of Vigo") 
Spain_2 <- pubs_authors_funder %>% find_text_filter("Toralla") 
Spain_3 <- pubs_authors_funder %>% find_text_filter("Plentziako Itsas Estazioa") 
Spain_4 <- pubs_authors_funder %>% find_text_filter("PiE-UPV/EHU") 
Spain_5 <- pubs_authors_funder %>% find_text_filter("Plentzia") 
Spain_6 <- pubs_authors_funder %>% find_text_filter("Spanish Bank of Algae") 
Spain_7 <- pubs_authors_funder %>% find_text_filter("University of Las Palmas de Gran Canaria") 
Spain_8 <- pubs_authors_funder %>% find_text_filter("University of the Basque Country") 
Spain_9 <- pubs_authors_funder %>% find_text_filter("Euskal Herriko Unibertsitatea") 
Spain_10 <- pubs_authors_funder %>% find_text_filter("Spain") 
Spain <- rbind(Spain_1, Spain_2, Spain_3, Spain_4, Spain_5, Spain_6, Spain_7,
               Spain_8, Spain_9, Spain_10)
Spain <- select(Spain, doi)
Spain <- unique(Spain)

Spain <- semi_join(pubs_authors, Spain)
Spain <- select(Spain, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                date, EMOBON_publication, abstract)

################################################################################
############################### PORTUGAL########################################
################################################################################

Portugal_1 <- pubs_authors_funder %>% find_text_filter("Centre of Marine Sciences") 
Portugal_2 <- pubs_authors_funder %>% find_text_filter("Centro de Ciencias do Mar") 
Portugal_3 <- pubs_authors_funder %>% find_text_filter("Coimbra Collection of Algae") 
Portugal_4 <- pubs_authors_funder %>% find_text_filter("Interdisciplinary Centre of Marine and Environmental Research") 
Portugal_5 <- pubs_authors_funder %>% find_text_filter("Centro Interdisciplinar de Investigação Marinha e Ambiental") 
Portugal_6 <- pubs_authors_funder %>% find_text_filter("Centro Interdisciplinar de Investigacao Marinha e Ambiental") 
Portugal_7 <- pubs_authors_funder %>% find_text_filter("Institute of Marine Research") 
Portugal_8 <- pubs_authors_funder %>% find_text_filter("Instituto do Mar") 
Portugal_9 <- pubs_authors_funder %>% find_text_filter("Campus de Gambelas Universidade do Algarve") 
Portugal_10 <- pubs_authors_funder %>% find_text_filter("Portugal") 
Portugal_11 <- pubs_authors_funder %>% find_text_filter("Centro de Ciências do Mar") 
Portugal <- rbind(Portugal_1, Portugal_2, Portugal_3, Portugal_4, Portugal_5, Portugal_6, 
                  Portugal_7, Portugal_8, Portugal_9, Portugal_10, Portugal_11)
Portugal <- select(Portugal, doi)
Portugal <- unique(Portugal)

Portugal <- semi_join(pubs_authors, Portugal)
Portugal <- select(Portugal, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                   date, EMOBON_publication, abstract)


################################################################################
############################### GREECE #########################################
################################################################################

Greece <- pubs_authors_funder %>% find_text_filter("Institute of Marine Biology, Biotechnology and Aquaculture") 
Greece <- select(Greece, doi)
Greece <- unique(Greece)

Greece <- semi_join(pubs_authors, Greece)
Greece <- select(Greece, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                 date, EMOBON_publication, abstract)


################################################################################
############################### ISRAEL #########################################
################################################################################

Israel_1 <- pubs_authors_funder %>% find_text_filter("Eilat") 
Israel_2 <- pubs_authors_funder %>% find_text_filter("Haifa") 
Israel_3 <- pubs_authors_funder %>% find_text_filter("Charney School of Marine Sciences") 
Israel_4 <- pubs_authors_funder %>% find_text_filter("Israel Oceanographic and Limnological Research") 
Israel_5 <- pubs_authors_funder %>% find_text_filter("Israel") 
Israel <- rbind(Israel_1, Israel_2, Israel_3, Israel_4, Israel_5)
Israel <- select(Israel, doi)
Israel <- unique(Israel)

Israel <- semi_join(pubs_authors, Israel)
Israel <- select(Israel, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                 date, EMOBON_publication, abstract)



################################################################################
############################### FRANCE #########################################
################################################################################

France_1 <- pubs_authors_funder %>% find_text_filter("Institut de la Mer de Villefranche") 
France_2 <- pubs_authors_funder %>% find_text_filter("Station Biologique de Roscoff") 
France_3 <- pubs_authors_funder %>% find_text_filter("France") 
France_4 <- pubs_authors_funder %>% find_text_filter("Observatoire Océanologique de Banyuls-sur-Mer") 
France_5 <- pubs_authors_funder %>% find_text_filter("Observatoire Oceanologique de Banyuls-sur-Mer") 
France <- rbind(France_1, France_2, France_3, France_4, France_5)
France <- select(France, doi)
France <- unique(France)

France <- semi_join(pubs_authors, France)
France <- select(France, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                 date, EMOBON_publication, abstract)


################################################################################
############################## FINLAND #########################################
################################################################################

Finland_1 <- pubs_authors_funder %>% find_text_filter("Tvärminne Zoological Station") 
Finland_2 <- pubs_authors_funder %>% find_text_filter("University of Helsinki") 
Finland_3 <- pubs_authors_funder %>% find_text_filter("University of Turku") 
Finland_4 <- pubs_authors_funder %>% find_text_filter("Finnish Environment Institute") 
Finland_5 <- pubs_authors_funder %>% find_text_filter("Finland") 
Finland_6 <- pubs_authors_funder %>% find_text_filter("Tvarminne Zoological Station") 
Finland_7 <- pubs_authors_funder %>% find_text_filter("The Archipelago Research Institute at Seili") 
Finland_8 <- pubs_authors_funder %>% find_text_filter("Husö Biological Station") 
Finland_9 <- pubs_authors_funder %>% find_text_filter("Huso Biological Station") 
Finland_10 <- pubs_authors_funder %>% find_text_filter("Archipelago Centre Korpoström") 
Finland_11 <- pubs_authors_funder %>% find_text_filter("Archipelago Centre Korpostrom") 
Finland_12 <- pubs_authors_funder %>% find_text_filter("Åbo Akademi University") 
Finland_13 <- pubs_authors_funder %>% find_text_filter("Abo Akademi University") 
Finland <- rbind(Finland_1, Finland_2, Finland_3, Finland_4, Finland_5, Finland_6, Finland_7, 
                 Finland_8, Finland_9, Finland_10, Finland_11, Finland_12, Finland_13)
Finland <- select(Finland, doi)
Finland <- unique(Finland)

Finland <- semi_join(pubs_authors, Finland)
Finland <- select(Finland, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                  date, EMOBON_publication, abstract)


################################################################################
############################### NORWAY #########################################
################################################################################

Norway_1 <- pubs_authors_funder %>% find_text_filter("University of Bergen") 
Norway_2 <- pubs_authors_funder %>% find_text_filter("Institute of Marine Research") 
Norway_3 <- pubs_authors_funder %>% find_text_filter("The Arctic University of Norway") 
Norway_4 <- pubs_authors_funder %>% find_text_filter("Norwegian University of Science and Technology") 
Norway_5 <- pubs_authors_funder %>% find_text_filter("University of Oslo") 
Norway_6 <- pubs_authors_funder %>% find_text_filter("Nofima") 
Norway_7 <- pubs_authors_funder %>% find_text_filter("Norwegian Institute for Water Research") 
Norway_8 <- pubs_authors_funder %>% find_text_filter("Tromsø Aquaculture Research Station") 
Norway_9 <- pubs_authors_funder %>% find_text_filter("Tromso Aquaculture Research Station") 
Norway <- rbind(Norway_1, Norway_2, Norway_3, Norway_4, Norway_5, Norway_6,
                Norway_7, Norway_8, Norway_9)
Norway <- select(Norway, doi)
Norway <- unique(Norway)

Norway <- semi_join(pubs_authors, Norway)
Norway <- select(Norway, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                 date, EMOBON_publication, abstract)


################################################################################
############################### BELGIUM ########################################
################################################################################

Belgium_1 <- pubs_authors_funder %>% find_text_filter("Ghent") 
Belgium_2 <- pubs_authors_funder %>% find_text_filter("Flanders Marine Institute") 
Belgium_3 <- pubs_authors_funder %>% find_text_filter("Hasselt") 
Belgium_4 <- pubs_authors_funder %>% find_text_filter("Leuven") 
Belgium_5 <- pubs_authors_funder %>% find_text_filter("Royal Belgian Institute of Natural Sciences")
Belgium <- rbind(Belgium_1, Belgium_2, Belgium_3, Belgium_4, Belgium_5)
Belgium <- select(Belgium, doi)
Belgium <- unique(Belgium)

Belgium <- semi_join(pubs_authors, Belgium)
Belgium <- select(Belgium, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                  date, EMOBON_publication, abstract)


################################################################################
################################ ITALY #########################################
################################################################################

Italy_1 <- pubs_authors_funder %>% find_text_filter("Stazione") 
Italy_2 <- pubs_authors_funder %>% find_text_filter("National Research Council") 
Italy_3 <- pubs_authors_funder %>% find_text_filter("National Institute of Oceanography and Applied Geophysics") 
Italy_4 <- pubs_authors_funder %>% find_text_filter("Istituto Nazionale di Oceanografia e di Geofisica Sperimentale") 
Italy_5 <- pubs_authors_funder %>% find_text_filter("Milano-Bicocca") #UNIMIB
Italy_6 <- pubs_authors_funder %>% find_text_filter("Federico") #UNINA
Italy_7 <- pubs_authors_funder %>% find_text_filter("Turin") #UNITO
Italy_8 <- pubs_authors_funder %>% find_text_filter("Torino") #UNITO
Italy_9 <- pubs_authors_funder %>% find_text_filter("CoNISMa")
Italy_10 <- pubs_authors_funder %>% find_text_filter("Consiglio Nazionale delle Ricerche") 
Italy_11 <- pubs_authors_funder %>% find_text_filter("Cluster Tecnologico Nazionale Blue Italian Growth") 
Italy_12 <- pubs_authors_funder %>% find_text_filter("Istituto Zooprofilattico Sperimentale del Piemonte Liguria") 
Italy_13 <- pubs_authors_funder %>% find_text_filter("Agenzia nazionale per le nuove tecnologie") 
Italy_14 <- pubs_authors_funder %>% find_text_filter("Italian National Agency for New Technologies") 
Italy_15 <- pubs_authors_funder %>% find_text_filter("Istituto Superiore per la Protezione e la Ricerca Ambientale") 
Italy_16 <- pubs_authors_funder %>% find_text_filter("degli Studi della Tuscia") 
Italy_17 <- pubs_authors_funder %>% find_text_filter("Cagliari") #UNICA
Italy_18 <- pubs_authors_funder %>% find_text_filter("Ferrara") #UNIFE
Italy_19 <- pubs_authors_funder %>% find_text_filter("Messina") #UNIME
Italy_20 <- pubs_authors_funder %>% find_text_filter("Marche Home") #UNIVPM
Italy <- rbind(Italy_1, Italy_2, Italy_3, Italy_4, Italy_5, Italy_6, Italy_7, Italy_8, Italy_9, 
               Italy_10, Italy_11, Italy_12, Italy_13, Italy_14, Italy_15, Italy_16, Italy_17, 
               Italy_18, Italy_19, Italy_20)
Italy <- select(Italy, doi)
Italy <- unique(Italy)

Italy <- semi_join(pubs_authors, Italy)
Italy <- select(Italy, title, journal, doi, volume, issue, page, url, ORCID, name, surname, 
                date, EMOBON_publication, abstract)

################################################################################
############################## SAVE RESULTS ####################################
################################################################################

write.table(Sweden, "Sweden_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Spain, "Spain_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Portugal, "Portugal_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Greece, "Greece_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Israel, "Israel_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(France, "France_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Finland, "Finland_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Norway, "Norway__pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Belgium, "Belgium_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

write.table(Italy, "Italy_pubs.tsv", 
            row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

################################################################################
################################################################################
################################################################################

save.image("Publications_per_node.RData") # creating ".RData" in current working directory

#Now you have everything in your computer, 
#and you can load it anytime you want by running
#load("Publications_per_node.RData")


