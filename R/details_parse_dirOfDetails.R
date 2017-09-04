parse_dirOfDetails<-function(DIR=choose.dir()){
  require(parallel);
  FLS<- list.files(DIR,recursive=T,pattern='details.xml',full.names = T);
  size.of.list <- length(FLS);
  cl <- makeCluster( min(size.of.list, detectCores()) );
  work<-parallel::parLapply(cl=cl,FLS,asyr::details_pull)
  Out<-Reduce('rbind',work[!grepl("FAILURE",work)])
  stopCluster(cl);
  Out
}
