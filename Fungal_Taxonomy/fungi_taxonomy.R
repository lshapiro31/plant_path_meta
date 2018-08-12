library(plyr)
library(dplyr)
library(reshape2)
library(taxize)

##get classification##

#read fungi species names from clipboard
fungi<-readClipboard()
#set up log file 
con <- file("gbif_classification_log.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
#pull taxonomy classification from GBIF
fungi_gbif<-classification(fungi, db="gbif")
#close log
sink()
sink(type="message")


##filter and reformat data##

#melt classification data
gbif_molten<-lapply(fungi_gbif, function(x) melt(x, id.vars="rank"))
#get fungi with no data on GBIF and filter them out
no_result<-gbif_molten[sapply(gbif_molten, nrow)<2]
gbif_molten<-gbif_molten[sapply(gbif_molten, nrow)>1]
#function to help rename for casting
wills_rename_function<-function(x){
  x$rank[x$variable=="id"]<-paste(x$rank, "id", sep="_")
  x$variable<-"name"
  return(x)
}
gbif_renamed<-lapply(gbif_molten, function(x) wills_rename_function(x))
#cast molten data into wide format
gbif_casted<-lapply(gbif_renamed, function(x) dcast(x, variable ~ rank, value.var = "value"))
#combine and order data
gbif_combined<-bind_rows(gbif_casted, .id="organism")
gbif_ordered<-gbif_combined[, c(1, 9, 13, 3, 11, 5, 7, 15, 10, 14, 4, 12, 6, 8, 16)]
