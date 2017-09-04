check<-function(d){
  xpathSApply(d, path = "//InspectionDetailsItem[Name='Result']//Details",xmlValue)!="OK"
}
