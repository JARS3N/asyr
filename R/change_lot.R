change_lot<-function(file,lot,fixed){
  require(XML)
  xml<-paste0(file,".xml")
  R.utils::gunzip(file,xml,remove=FALSE,overwrite=T)
  x <- xmlParse(xml)
  n1 <- xpathApply(x,"//Cartridge//Lot//text()")
  n2 <- xpathApply(x,"//CartridgeBarcode//text()")
  n3 <- xpathApply(x,"//Cartridge//Barcode//text()")
  sapply(n1,function(G){
    text1 <- gsub(lot,fixed,xmlValue(G))
    xmlValue(G) <- text1
  })
  sapply(n2,function(H){
    text2 <- gsub(paste0(lot,"B"),paste0(fixed,"B"),xmlValue(H))
    xmlValue(H) <- text2
  })
  sapply(n3,function(I){
    text3 <- gsub(paste0(lot,"B"),paste0(fixed,"B"),xmlValue(I))
    xmlValue(I) <- text3
  })
  XML::saveXML(x,xml)
  R.utils::gzip(xml,gsub("[.]asyr","_fixed.asyr",file))
}
