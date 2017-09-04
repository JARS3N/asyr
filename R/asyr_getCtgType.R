getCtgTypefromInst<-function(Z){
  Instperfix<-substr(getInst(Z),1,2)
  lettr<-c("Q"=10,"W"=20,"C"=43,"W"=41,"B"=42)
  names(lettr[lettr==Instperfix])
}

getCtgType<-function(Z){
  Ltr<- xpathSApply(Z,"//Cartridge//Type",xmlValue)
  if (length(Ltr)==0){
    Ltr<-getCtgTypefromInst(Z)
  }
  Ltr
}

