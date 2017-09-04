determineAssay<-function(X){
fl <- getFile(X)
 u<- c("Gain"=grepl("GAIN",toupper(fl)),"Ksv"=grepl("KSV",toupper(fl)))
 names(u[u==TRUE])
}

