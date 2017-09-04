pH_coefs<-function(X){
  coefs<-list(
    "slope"= as.numeric(xpathSApply(X,"//Item[Key='pH']//Value//AnalyteCalibration//GainEquation//C3",xmlValue)
    ),
    "intercept"=as.numeric(xpathSApply(X,"//Item[Key='pH']//Value//AnalyteCalibration//GainEquation//C4",xmlValue)
    ),
    "target"=as.numeric(xpathSApply(X,"//Item[Key='pH']//Value//AnalyteCalibration//TargetEmissionValue",xmlValue))
  )
  coefs$gain<-c((coefs$slope * coefs$target)+ coefs$intercept);
  coefs
}
O2_coefs<-function(X){
  ksvs<-as.numeric(xpathSApply(X,'//O2DataModifiers//Ksv',xmlValue))
  coefs<- list(
    "target"=as.numeric(xpathSApply(X,"//Item[Key='O2']//Value//AnalyteCalibration//TargetEmissionValue",xmlValue)),
   "Ksv"=ksvs[1],
   "CorrectedKsv"=ksvs[2]
  )
  coefs
}


