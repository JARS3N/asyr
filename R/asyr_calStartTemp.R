calStartTemp<-function(u){
  as.numeric(xpathSApply(u,"//CalibrationStartTemperature",xmlValue))
}
