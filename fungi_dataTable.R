## merging USDA data with taxonomy data ##
#start w/ taxonomy dataframe and fungi_data list#

library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(shiny)

#read in taxonomy data
fungi_taxonomy <- read.csv("fungi_taxonomy.csv", header = T)

#filter any fungi not shared between fungi_data and fungi_taxonomy
fungi_taxonomy2<-fungi_taxonomy[which(fungi_taxonomy$organism %in% names(fungi_data)),]
fungi_data2<-fungi_data[names(fungi_data) %in% fungi_taxonomy2$organism]

#extract basic data from fungi_data, add index value
fungi_basic_data<-lapply(fungi_data2, "[[", 2)

#remove entries with no basic_data
fungi_basic_data_notempty<-fungi_basic_data2[sapply(fungi_basic_data2, nrow)>0]
fungi_taxonomy2<-fungi_taxonomy2[fungi_taxonomy2$organism %in% names(fungi_basic_data_notempty),]

#cast basic_data into wide format, rbind into dataframe
fungi_basic_data3<-lapply(fungi_basic_data_notempty, function(x) dcast(x, id ~ attribute, value.var = "value"))
basic_data_df<-rbind.fill(fungi_basic_data3)
basic_data_df<-basic_data_df[,2:5]

#combine taxonomy and basic_data
master_data_df<-cbind(fungi_taxonomy2, basic_data_df)

#make links to gbif for each species
master_data_df$url<-paste0("https://www.gbif.org/species/", master_data_df$species_id)
master_data_df$species_id<-paste0("<a href='", master_data_df$url, "' target='_blank'>", master_data_df$species_id, "</a>")
master_data_df<-master_data_df[,-c(20)]

#Clean up columns order and names for datatable
fungi_for_DT<-master_data_df[,c(1, 15:19, 2:6)]
colnames(fungi_for_DT)<-c("Organism", "GBIF ID", "Disease Type", "Distribution", "Host", "Substrate", "Kingdom", "Phylum", "Class", "Order", "Family")



