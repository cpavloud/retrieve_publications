# How to retrieve scientific publications based on specific keywords
This is a repository containing the code and instructions on how to retrieve and format EMBRC scientific publications, which are later used to populate the [publications section of the EMBRC website](https://www.embrc.eu/publications/) and to calculate relevant Key Performance Indicators (KPIs).

What should be taken into account is that it is very difficult to retrieve a list of all the publications, without missing anything and without having false positives. 

This is because not all journals are indexed in Web Of Science, Scopus or Pubmed. 
Searching Google Scholar is one solution, but unfortunately Google Scholar does not have an API. Also, false positives results might be retrieved, in which case manual filtering is needed to remove them. 

* Step 1:

  Search based on given keywords, retrieve publications and save the results as an output.

  This can be done either by downloading [Publish or Perish](https://harzing.com/resources/publish-or-perish) and searching on Google Scholar or by using the [Retrieve_publications.R](https://github.com/cpavloud/retrieve_publications/blob/main/Retrieve_publications.R) script and searching in [Crossref](https://www.crossref.org) and [Semantic Scholar](https://www.semanticscholar.org).

  If you use the first option, there might be false positives retrieved which would then require filtering. Also, in most cases, the retrieved information is not displayed properly (e.g. authors names, journals, dois etc. are not provided in full). 

  If you use the second option, the results in some cases will be very few since the script searches in for keywords in the title of the publications and in the publication metadata (e.g. dois, acknowlegments, funding etc.) and not in the full text. So, for example, in the case of EMO BON, publications will be retrieved only if the EMO BON is mentioned in the title.

  Please bare in mind that, either way, you should select the proper start year and end year for the publications retrieval. 

* Step 2:

  If you have used the [Retrieve_publications.R](https://github.com/cpavloud/retrieve_publications/blob/main/Retrieve_publications.R) script, then continuing in the next step is more straightforward. By running the [Create_file_for_website.R]() script, you will have your results as they should be delivered to the communications team for display on the website.

  If you have used [Publish or Perish](https://harzing.com/resources/publish-or-perish) to search and export your results (in .csv), you should proceed with the [Create_file_for_website_perish.R]() script.


* Step 3



* Step 4
