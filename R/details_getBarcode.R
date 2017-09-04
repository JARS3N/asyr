getBarcode<-function(d){
  xpathSApply(d,path = "//InspectionDetailsItem[Name='Bar Code']//Details",xmlValue)
}
