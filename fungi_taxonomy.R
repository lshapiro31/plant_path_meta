##get classification
con <- file("sinktest.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
sink_test_classified<-classification(fungi_sinktest, db="gbif")
sink()
sink(type="message")

##single dataframe test
molten_test<-melt(gbif_hierarchy_1_test[[1]], id.vars = "rank")
molten_test$rank[molten_test$variable=="id"]<-paste(molten_test$rank, "id", sep="_")
molten_test$variable<-"name"
dcast(molten_test, variable ~ rank, value.var = "value")

##apply method to list
melt_list<-lapply(gbif_hierarchy_1_test, function(x) melt(x, id.vars="rank"))
melt_list<-melt_list[sapply(melt_list, nrow)>0]
wills_rename_function<-function(x){
  x$rank[x$variable=="id"]<-paste(x$rank, "id", sep="_")
  x$variable<-"name"
  return(x)
}
melt_list_renamed<-lapply(melt_list, function(x) wills_rename_function(x))
casted_list<-lapply(melt_list_renamed, function(x) dcast(x, variable ~ rank, value.var = "value"))
combined_casted<-bind_rows(casted_list, .id="organism")
ordered_combined_casted<-combined_casted[, c(1, 9, 13, 3, 11, 5, 7, 15, 10, 14, 4, 12, 6, 8, 16)]