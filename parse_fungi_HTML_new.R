##***IMPORTANT*** MUST OPEN WITH UTF-8 ENCODING, open script then go file->reopen with encoding->UTF-8 ##
library(XML)
library(stringr)
library(data.table)
library(tidyr)
library(reshape2)
library(ggmap)
library(sf)
library(mapview)
library(pbapply)
library(leaflet)
library(abind)
library(htmltools)

#setwd to folder with .html files
setwd()

files<-list.files(getwd(), pattern="*.html")


#reads html from USDA files and makes list with all data organized in arrays
parse_fungi_html <- function(x) {
  #parse html into text and pull out all paragraph tags
  html_doc <- htmlTreeParse(x, useInternalNodes = T)
  html_text <- unlist(xpathApply(html_doc, '//p', xmlValue))
  
  #get basic data
  basic_data <- data.frame(html_text[grepl("Distribution:|Substrate:|Disease Note:|Host:", html_text)])
  basic_data <- separate(basic_data, 1, into = c("attribute", "value"), sep = ":")
  basic_data$value <- gsub(basic_data$value, pattern = "(\\.[^.]*)$", replacement = "")
  
  #get references and associated codes and store in df
  lit <- html_text[which(grepl("Literature database", html_text)) : which(grepl("Specimens database", html_text))]
  lit <- lit[-c(1, length(lit), length(lit)-1)]
  lit <- strsplit(lit, "\\s(?=\\S*$)", perl=T)
  lit_df <- do.call(rbind.data.frame, lit)
  colnames(lit_df) <- c("reference", "code")
  lit_df$code <- gsub("[()]", "", lit_df$code)
  lit_df$reference <- as.character(lit_df$reference)
  
  #get nomenclature data and clean up synonyms
  html_text2 <- html_text[-c(1:4)]
  nomenclature <- html_text2[(which(grepl("Nomenclature data", html_text2))) : (which(grepl("Distribution", html_text2)))]
  nomenclature <- nomenclature[-c(length(nomenclature))]
  nomenclature <- gsub(nomenclature, pattern = "Â|â|‰|=|¡", replacement = "")
  nomenclature <- str_trim(nomenclature, side = "both")
  nomenclature <- gsub(nomenclature, pattern = "Nomenclature data for ", replacement = "")
  
  #get location information from html text and remove empty entries
  locations <- html_text2[(which(grepl("Fungus-Host", html_text2))) : which(grepl("Literature database", html_text2))]
  locations <- locations[-c(1, length(locations), length(locations)-1)]
  
  locations <- locations[-(grep("\r\n\t\t\t", locations))]
  
  locations <- gsub(locations, pattern = "\\(([^\\)]+)\\)", replacement = "")
  locations <- gsub(locations, pattern = "\\*", replacement = "")
  locations <- gsub(locations, pattern = ";\\s", replacement = ",")
  locations <- gsub(locations, pattern = "ontario", replacement = "Ontario")
  locations <- gsub(locations, pattern = "Abyssinics", replacement = "abyssinics")
  locations <- gsub(locations, pattern = "Andreana", replacement = "andreana")
  locations <- gsub(locations, pattern = "Arborescens", replacement = "arborescens")
  locations <- gsub(locations, pattern = "Aristata", replacement = "aristata")
  locations <- gsub(locations, pattern = "Basilaris", replacement = "basilaris")
  locations <- gsub(locations, pattern = "Baumannii", replacement = "baumannii")
  locations <- gsub(locations, pattern = "Berberis Ilicifolia", replacement = "berberis ilicifolia")
  locations <- gsub(locations, pattern = "Briotii", replacement = "briotii")
  locations <- gsub(locations, pattern = "Canaertii", replacement = "canaertii")
  locations <- gsub(locations, pattern = "Chia", replacement = "chia")
  locations <- gsub(locations, pattern = "Cicadellidae", replacement = "cicadellidae")
  locations <- gsub(locations, pattern = "Condensata", replacement = "condensata")
  locations <- gsub(locations, pattern = "Conspicuus", replacement = "conspicuus")
  locations <- gsub(locations, pattern = "Cordyluridae:", replacement = "cordyluridae")
  locations <- gsub(locations, pattern = "Densa", replacement = "densa")
  locations <- gsub(locations, pattern = "Diptera", replacement = "diptera")
  locations <- gsub(locations, pattern = "Dregeanus", replacement = "dregeanus")
  locations <- gsub(locations, pattern = "Elegantissima", replacement = "elegantissima")
  locations <- gsub(locations, pattern = "Glabrata", replacement = "glabrata")
  locations <- gsub(locations, pattern = "Glauca", replacement = "glauca")
  locations <- gsub(locations, pattern = "Globosa", replacement = "globosa")
  locations <- gsub(locations, pattern = "Grandiflora", replacement = "grandiflora")
  locations <- gsub(locations, pattern = "Homoptera", replacement = "homoptera")
  locations <- gsub(locations, pattern = "Hymenoptera", replacement = "hymenoptera")
  locations <- gsub(locations, pattern = "Islands", replacement = "islands")
  locations <- gsub(locations, pattern = "Japonicus:", replacement = "japonicus")
  locations <- gsub(locations, pattern = "Japonicus", replacement = "japonicus")
  locations <- gsub(locations, pattern = "Lalandei", replacement = "lalandei")
  locations <- gsub(locations, pattern = "Leporinum", replacement = "Leporinum")
  locations <- gsub(locations, pattern = "Longipinnatus", replacement = "longipinnatus")
  locations <- gsub(locations, pattern = "Mantica", replacement = "mantica")
  locations <- gsub(locations, pattern = "Pteridophyta", replacement = "pteridophyta")
  locations <- gsub(locations, pattern = "Pubens", replacement = "pubens")
  locations <- gsub(locations, pattern = "Pyramidalis", replacement = "pyramidalis")
  locations <- gsub(locations, pattern = "Recurvifolia", replacement = "recurvifolia")
  locations <- gsub(locations, pattern = "Rotundifolia", replacement = "rotundifolia")
  locations <- gsub(locations, pattern = "Rough", replacement = "rough")
  locations <- gsub(locations, pattern = "Spp", replacement = "spp")
  locations <- gsub(locations, pattern = "Squarrosa :", replacement = "squarrosa")
  locations <- gsub(locations, pattern = "Suffruticosa", replacement = "suffruticosa")
  locations <- gsub(locations, pattern = "Thunbergii", replacement = "thunbergii")
  locations <- gsub(locations, pattern = "Tripartita", replacement = "tripartita")
  locations <- gsub(locations, pattern = "Vittatum", replacement = "vittatum")
  locations <- gsub(locations, pattern = "Perennis", replacement = "perennis")
  locations <- gsub(locations, pattern = "Rica", replacement = "Costa Rica")
  locations <- gsub(locations, pattern = "Costa Costa Rica", replacement = "Costa Rica")
  locations <- gsub(locations, pattern = "USSR", replacement = "Poland")
  locations <- gsub(locations, pattern = "South Africa Unknown", replacement = "South Africa")
  locations <- gsub(locations, pattern = "Czechoslovakia", replacement = "Czech Republic")
  locations <- gsub(locations, pattern = "Dakota", replacement = "North Dakota")
  locations <- gsub(locations, pattern = "Pacific islands", replacement = "Polynesia")
  locations <- gsub(locations, pattern = "Northwestern States", replacement = "Pacific Northwest States")
  locations <- gsub(locations, pattern = "Southwestern States", replacement = "Southwest United States")
  locations <- gsub(locations, pattern = "Tropical America", replacement = "Central America")
  locations <- gsub(locations, pattern = "Jersey", replacement = "New Jersey")
  locations <- gsub(locations, pattern = "New New Jersey", replacement = "New Jersey")
  locations <- gsub(locations, pattern = "Zambia zim", replacement = "Zambia")
  locations <- gsub(locations, pattern = "Gulf states", replacement = "United States Gulf Coast")
  locations <- gsub(locations, pattern = "Java", replacement = "Indonesia Java")
  locations <- gsub(locations, pattern = "Indonesia Indonesia Java", replacement = "Indonesia Java")
  locations <- gsub(locations, pattern = "North North Dakota", replacement = "North Dakota")
  
  
  locations <- gsub(locations, pattern = "Congo\\, Democratic Republic of the", replacement = "Congo")
  locations <- gsub(locations, pattern = "Georgia\\, Republic of", replacement = "Republic of Georgia")
  locations <- gsub(locations, pattern = "Christmas Island\\, Territory of", replacement = "Christmas Island")
  locations <- gsub(locations, pattern = "Congo\\, Republic of the", replacement = "Congo")
  
  
  if(length(locations) > 0) {
    
    #get list of hosts
    hosts <- str_extract(locations, ".+?\\s(?=[A-Z])")
    hosts <- gsub(hosts, pattern = ":", replacement = "")
    
    #split locations into separate columns
    locations <- sub(".+?\\s(?=[A-Z])", "\\1", locations, perl = T)
    locations <- strsplit(locations, "\\,(?=[A-Z]+)", perl = T)
    max.length <- max(sapply(locations, length))
    locations_filled <- lapply(locations, function(x) { c(x, rep(NA, max.length-length(x)))})
    locations_df <- do.call(rbind.data.frame,locations_filled)
    locations_df[] <- lapply(locations_df, as.character)
    
    #separate literature codes from location data, put into df, and clean up
    lit_codes <- lapply(locations_df, strsplit, "-")
    lit_codes <- lapply(lit_codes, gsub, pattern = "[^0-9 | ^,]", replacement = "")
    lit_codes_df <- do.call(cbind.data.frame,lit_codes)
    lit_codes_df[] <- lapply(lit_codes_df, as.character)
    lit_codes_df[] <- lapply(lit_codes_df, gsub, pattern = ",", replacement = "")
    lit_codes_df[] <- lapply(lit_codes_df, function(x) trimws(x, which = "both"))
    lit_codes_df[] <- lapply(lit_codes_df, gsub, pattern = " ", replacement = ",")
    
    #remove codes and clean up location names
    locations_df[] <- lapply(locations_df, gsub, pattern = ",", replacement = "")
    locations_df[] <- lapply(locations_df, gsub, pattern = "-", replacement = "")
    locations_df[] <- lapply(locations_df, gsub, pattern = "[0-9]", replacement = "")
    locations_df[] <- lapply(locations_df, gsub, pattern = "\\scard", replacement = "")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Petr\\.\\)", replacement = "")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Brunei Darussalam", replacement = "Brunei")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Leporinum Australia", replacement = "Australia")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Leporinum New Zealand", replacement = "New Zealand")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Columbia", replacement = "Colombia")
    locations_df[] <- lapply(locations_df, gsub, pattern = "District of Colombia", replacement = "District of Columbia")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Cote d''Ivoire", replacement = "Cote d'Ivoire")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Faeroe", replacement = "Faroe")
    locations_df[] <- lapply(locations_df, gsub, pattern = "GuineaBissau", replacement = "Guinea")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Himalaya", replacement = "Himalayas")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Himalayass", replacement = "Himalayas")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Italy Sicily", replacement = "Sicily")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Malay Peninsula", replacement = "Malaysia")
    locations_df[] <- lapply(locations_df, gsub, pattern = "Russia Siberia", replacement = "Siberia")
    
    
    locations_df[] <- lapply(locations_df, function(x) str_trim(x, side = "both"))
    
    #combine location and codes into one dataframe and add host data
    colnames(locations_df) <- paste0("location", 1:ncol(locations_df))
    colnames(lit_codes_df) <- paste0("code", 1:ncol(locations_df))
    master_df <- cbind(locations_df, lit_codes_df)
    master_df$host <- hosts
    master_df <- master_df[, c(ncol(master_df), 1:(ncol(master_df)-1))]
    
    #melt locations/codes data
    locations_molten <- melt(master_df, id = "host")
    locations_molten2 <- locations_molten[grep("code", locations_molten$variable), ]
    locations_molten <- locations_molten[grep("location", locations_molten$variable), ]
    locations_molten$code <- locations_molten2$value
    locations_molten <- locations_molten[, c(1, 3, 4)]
    
    #combine lit codes into one column for each unique host/location pair
    locations_aggregated <- aggregate(data = locations_molten, code ~ host + value, paste, collapse = ",")
    
    #add numerical index for how many times each location appears
    setDT(locations_aggregated)
    locations_aggregated[, indx:=1:.N, by = value]
    setDF(locations_aggregated)
    
    #split multiple lit codes into separate columns
    max_code <- strsplit(locations_aggregated$code, ",")
    max_code_length <- max(lengths(max_code))
    locations_aggregated <- separate(locations_aggregated, code, into = paste("code", 1:max_code_length, sep = "_"), sep = ",", convert = T)
    
    #cast locations into wide format
    cast_locations <- dcast(locations_aggregated, value ~ indx, value.var = "host")
    
    #cast codes into wide format, separate dataframe for each "level" of code
    codes_list <- list()
    for(i in 1:max_code_length){
    codes_list[[i]] <- dcast(locations_aggregated, value ~ indx, value.var = paste("code", i, sep = "_"))
    }
    
    #make 3 dimensional array with first level being location/host pairs, and levels behind being associated literature codes
    locations_list <- c(list(cast_locations), codes_list)
    locations_array <- array(unlist(locations_list), dim = c(dim(locations_list[[1]]), length(locations_list)))
    
    
    
    #collect all data into list
    return(list(nomenclature = nomenclature, basic_data = basic_data, references = lit_df, locations_hosts = locations_array))
  
    } else {
    
      return(list(nomenclature = nomenclature, basic_data = basic_data, references = lit_df, locations_hosts = "No location or host data"))
  }
}

#name the list with fungi names
fungi_data <- pblapply(files, parse_fungi_html)
fungi_names <- sapply(sapply(fungi_data, "[[", 1), "[[", 1)
names(fungi_data) <- fungi_names

#make dataframe with unique locations for lookup
fungi_locations<-lapply(fungi_data, "[[", 4)
locations<-lapply(fungi_locations, '[', ,1,1)
locations<-unlist(locations, use.names = F)
locations<-unique(locations)
locations<-data.frame(location=locations, stringsAsFactors = F)


#dummy example
test_array<-test[[4]]
test_geocodes<-geocodes[which(test_array[,1,1] %in% geocodes$location),]
test_dims<-dim(test_array)
test_geocodes_list<-list(test_geocodes[,2:3])
for(i in 2:test_dims[3]){
  test_geocodes_list[[i]]<-data.frame("1"=rep(NA, test_dims[1]), "2"=rep(NA, test_dims[1]))
}
test_geocodes_array<-array(unlist(test_geocodes_list), dim=c(dim(test_geocodes_list[[1]]), length(test_geocodes_list)), dimnames = list(1:test_dims[1], c("lon", "lat"), 1:test_dims[3]))
test_bound_array<-abind(test_array, test_geocodes_array, along=2)


paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

mapping_df<-data.frame(test_bound_array[,,1], stringsAsFactors = F)
mapping_df$lon<-as.numeric(mapping_df$lon)
mapping_df$lat<-as.numeric(mapping_df$lat)
df_args<-c(mapping_df[,4:ncol(mapping_df)-2], sep=", ", na.rm=T)
mapping_df$hosts<-do.call(paste5, df_args)


paste(sep=", ", mapping_df[2:ncol(mapping_df)-2])

map <- leaflet(data = mapping_df) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup=mapping_df$label)


mapping_df$label<-paste(sep="<br/>", paste("Location:", mapping_df$V1, sep=" "), 
                                            paste("Hosts:", mapping_df$hosts, sep=" "))
                        

##random stuff
fungi_locations[which(grepl(fungi_locations, "Rica"))]

fungi_nomenclature<-lapply(fungi_data, "[[", 1)
fungi_nomenclature<-unlist(fungi_nomenclature)
fungi_nomenclature<-unique(fungi_nomenclature)
fungi_nomenclature<-data.frame(name=fungi_nomenclature)

fungi_data$`Alternaria carotae`$geocodes<-data.frame(locations=fungi_data$`Alternaria carotae`$locations_hosts[,1,1], stringsAsFactors = F)
geocodes<-fungi_data$`Alternaria carotae`$geocodes
geocodes[]<-lapply(geocodes, gsub, pattern="USSR", replacement="Sovient Union")

register_google(key="AIzaSyDgaq94-9mgIRR-i_sY-ZoibAJLRsGOpzI")
geocodes<-mutate_geocode(locations, location)
geocode_sf <- st_as_sf(geocodes, coords = c("lon", "lat"), crs = 4326)
mapview(geocode_sf)

map <- leaflet(data = geocodes) %>%
  addTiles() %>%
  addLabelOnlyMarkers(~lon, ~lat, label = ~as.character(location), labelOptions = labelOptions(noHide = T, direction = 'top'))




