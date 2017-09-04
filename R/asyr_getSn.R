getSn<-function(X){
  sn <- xpathSApply(X,"//Cartridge//Serial",xmlValue)
  x<-getFile(X)
  regexp<-"_[0-9]{1,3}_"
  if(length(sn)==0){
    sn<- unlist(regmatches(x, gregexpr(regexp, x)))
    sn<-gsub("_","",sn)
    return(sn)
  }else{
    return(sn)}
}


