CalDat<-function(fl=file.choose()){
require(XML)
xml<-XML::xmlTreeParse(fl,useInternalNodes = T)
pull<-function(xml,analyte){
  ans<-c("pH"="//PHData//",
         "O2"="//O2Data//"
         )[analyte]

partial_xpaths<-c("IntialReferenceDeltaValues//ArrayOfInt//int",
"LedStatusValues//ArrayOfCalibrationQuality//CalibrationQuality",
"LedValues//ArrayOfInt//int",
"CalibrationEmissionValues//ArrayOfDouble//double")
XPATHS<-vapply(ans,FUN=function(u){paste0(u,partial_xpaths)},FUN.VALUE = character(4))
    combine_to_df<-function(u){
      data.frame("RefDelta"=as.numeric(u[[1]]),
               "Status" = as.character(u[[2]]),
               "LED" = as.numeric(u[[3]]),
               "CalEmission" = as.numeric(u[[4]])
               )
    }
data_lists<-lapply(XPATHS,xpathSApply,doc=xml,fun=xmlValue)
df<-combine_to_df(data_lists)
setNames(df,paste(analyte,names(df),sep="."))
}
getType<-function(L){
  as.vector(setNames(c("C","B","W"),c(8,24,96))[as.character(L)])
}
getWells<-function(Type){
  list(
    "W"= paste0(sapply(LETTERS[1:8],rep,12),sprintf("%02d",c(1:12))),
    "B" = paste0(sapply(LETTERS[1:4],rep,6),sprintf("%02d",c(1:6))),
    "C" = paste0(LETTERS[1:8],"01")
  )[[Type]]
}
joined_df<-cbind(pull(xml,'pH'),pull(xml,'O2'))
joined_df$type<-getType(nrow(joined_df))
joined_df$Well<-getWells(unique(joined_df$type))
Lot<-try(xpathSApply(xml,"//CartridgeLot",xmlValue))
sn<-try(xpathSApply(xml,"//CartridgeSerial",xmlValue))
if(length(Lot)==0){
  Lot<-NA
}else{
  Lot<-paste0(unique(joined_df$type),Lot)
  }
if(length(sn)==0){sn<-NA}
joined_df$Lot<-Lot
joined_df$sn<-sn
joined_df$fl<-fl
joined_df
}
