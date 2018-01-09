


asyr_grab96<-function(u){
  require(dplyr)
  pH<-c(rep(3.8,12),rep(5,12),rep(5.8,12),rep(6.6,12),rep(7.0,12),rep(7.4,12),rep(8.15,12),rep(9.2,12))
  u %>%
    .$LVL %>%
    filter(.,Tick %in% tickfilter_a(Tick)) %>%
    dplyr::select(.,counts=pHlvl,Tick,Well) %>%
    dplyr::mutate(.,pH=pH[as.numeric(factor(Well))]) %>%
    merge(.,data.frame(dye=c(rep('CL',6),rep('PR',6)),Well=unique(.['Well'])),by="Well") %>%
    dplyr::group_by(.,Well,pH,dye) %>%
    dplyr::summarise(.,counts=mean(counts)) %>%
    dplyr::mutate(.,fl=u[['file']])
}


#####################
asyr_grabXFp<-function(u){
  require(dplyr)
  pH<-c(3.8,5,5.8,6.6,7.0,7.4,8.15,9.2)
  u %>%
    .$LVL %>%
    dplyr::select(.,counts=pHlvl,Tick,Well)%>%
    dplyr::filter(.,Tick %in% tickfilter_b(Tick)) %>%
    dplyr::mutate(.,Tick=as.numeric(factor(Tick))) %>%
    dplyr::mutate(.,dye = c("CL","PR")[as.numeric(factor(Tick>3))],pH=pH[as.numeric(factor(Well))]) %>%
    dplyr::group_by(.,Well,pH,dye) %>%dplyr::summarise(.,counts=mean(counts)) %>%
    dplyr::mutate(.,fl=u[['file']])
}

################################
asyr_grab24<-function(u){
  require(dplyr)
  pH=c(rep(3.8,3),rep(5,3),rep(5.8,3),rep(6.6,3),rep(7.0,3),rep(7.4,3),rep(8.15,3),rep(9.2,3))
  u %>%
    .$LVL %>%
    dplyr::select(.,counts=pHlvl,Tick,Well)%>%
    dplyr::filter(.,Tick %in% tickfilter_b(Tick)) %>%
    dplyr::mutate(.,Tick=as.numeric(factor(Tick))) %>%
    dplyr::mutate(.,dye = c("CL","PR")[as.numeric(factor(Tick>3))],pH=pH[as.numeric(factor(Well))]) %>%
    dplyr::group_by(.,Well,pH,dye) %>%
    dplyr::summarise(.,counts=mean(counts)) %>%
    dplyr::mutate(.,fl=u[['file']])
}

##################
asyr_mungelist<-list(asyr_grab96,asyr_grab24,asyr_grabXFp)
####function to run pKa
###################


asyr_createRmd<-function (pHFluor, MFBatch, Directory) {
  file.path(Directory,"data.feather") %>%
    file.exists(.) %>%
    if(.){DATA<<- "data.feather"}else{DATA<<- "data.csv"}
  fp<-shQuote(file.path(Directory, DATA))
  imp<-paste('rio::import(',fp,')')
  readLines(system.file("rmd/pKaTemplate.Rmd", package = "shinyStar")) %>%
    gsub("XBATCHX", MFBatch, .) %>%
    gsub("XLOTX", pHFluor, .) %>%
    gsub("rio::import('data.csv')",imp ,.)
}


asyr_pKa<-function(pHFluor,MFBatch,Platform,Directory){
  FileOut<-file.path(Directory,paste0(pHFluor,"pKa.Rmd"))
  list.files(path=Directory,pattern='asyr',full.names = TRUE)   %>%
    lapply(.,XML::xmlTreeParse,useInternalNodes=T)  %>%
    lapply( .,process) %>%
    lapply(.,asyr_mungelist[[as.numeric(Platform)]]) %>%
    dplyr::bind_rows() %>%
    write.csv(x=.,file=file.path(Directory,"data.csv"),row.names=F)
  asyr_createRmd(pHFluor,MFBatch,Directory) %>%
    writeLines(text=.,con=file.path(Directory,paste0(pHFluor,"pKa.Rmd")),sep="\n")
  rmarkdown::render(input=FileOut)
}
