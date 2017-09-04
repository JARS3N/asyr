
getLot<-function(X){
  Lot<-paste0(xpathSApply(X,"//Cartridge//Type",xmlValue),
              xpathSApply(X,"//Cartridge//Lot",xmlValue))
  if (length(Lot)==0){
    flx<-getFile(X)
    regexp<-"[W|B|C|Q|T]{1}[E|0-9]{1}[0-9]{4}"
    Lot<- paste0(unlist(regmatches( flx, gregexpr(regexp, flx))))
  }
  if(Lot!=""){
  return(Lot)
  }else{
      return(NA)
    }
}