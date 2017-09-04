swversion<-function(x){
  xpathSApply(x,"//SWVersion",xmlValue)
}