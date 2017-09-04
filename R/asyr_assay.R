assay<-function(X){
  AL<-list("Gain"=asyr::newGain,"Ksv"=asyr::Ksv)
 merge(AL[[X$assay]](X),data.frame(sn=X$sn,Inst=X$Inst,Lot=X$Lot))
}

whichAssay<-function(X){
  type<-substr(X$Lot,1,1)
  if(type=="C"){
    asyr::ComboAssay(X)
  }else{
    asyr::assay(X)
  }
}
