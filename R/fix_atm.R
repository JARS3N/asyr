fix_atm <- function(u) {
library(XML)
  zip_name <- gsub("xflr", "zip", u)
  #just check if the asyr exists first

  file.copy(u, zip_name)
  unzip(zipfile=zip_name,"__ASSAYDATA__")
  X<-XML::xmlTreeParse("__ASSAYDATA__",useInternalNodes=T)
  
 atm<- xpathSApply(X,"//AtmosphericPressure/text()")
 temptol<- xpathSApply(X,"//EnvironmentDataModifiers/TemperatureTolerance/text()")
 
 sapply(atm,function(H){xmlValue(H) <- "760"})
 sapply(temptol,function(H){xmlValue(H) <- "0.4"})
 
 XML::saveXML(doc=X,file="__ASSAYDATA__")

 zip(zip_name,files="__ASSAYDATA__",flags="-ugr")
 unlink("__ASSAYDATA__")
 file.rename(zip_name,gsub("[.]zip","_fixed.xflr",zip_name))
}
