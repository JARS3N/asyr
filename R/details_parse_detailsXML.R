
parse_detailsXML<-function(doc){
d<-xmlTreeParse(doc,useInternalNodes =T)
if(check(d)){return("FAILURE")}
barcode<-getBarcode(d)
tbl<-findTable(d)
htMLtREE<-XML::htmlTreeParse(tbl,useInternalNodes = T)
tds<-xpathApply(htMLtREE, path = "//td")
strs<-xmlApply(tds,getChildrenStrings,len=48)
dfs<-Reduce('rbind',lapply(strs,pull_cell_data))
dfs$Lot<-paste0(substr(barcode,1,1),substr(barcode,7,11))
dfs$sn<-substr(barcode,2,6)
dfs$plat<-getPlat(substr(barcode,1,1))
dfs[,c("Well","Lot","sn","plat" ,names(dfs)[1:20])]
}

