Fungi list from Lori was loaded into R. 

The "taxize" package was used to find taxonomic hierarchy data for each species. 

The Global Biodiversity Information Facility (GBIF) was queried, as NCBI and ITIS do not have data for most of these species.

In many cases, the GBIF has multiple entries that match the query. Here, I picked the first entry, as there were so many that this was necessary to save time. These cases and the selections are clear in the log file. 

In some cases, there were no matches in GBIF. These results are listed in the "no_results" file. 

In the final taxonomy tabular file, the original query name is listed under "organism" column, then all the hierarchy information, followed by GBIF IDs for each hierarchy level, and finally the species name matched in GBIF.

**Important Note**
Many of the query names don't match the found species name. This is not a problem, it just reflects that for many of these species there are multiple accepted names, or the names have been updated.

For many species, they are missing some levels of taxonomy (listed as NA) because this info was not found in GBIF. The table can be filtered to find which species are missing a critical level and it can be searched for manually if needed. 

