UploadsCC<-function(u){
  require(RMySQL)
  platstring<-c(
    "C" ="xfpwetqc",
    "B" = "xfe24wetqc",
    "W" = "xfe96wetqc",
    "Q" = "xf24legacy"
  )
  #ConnectInfo<-DataStash::Triton()
  my_db <- rmysqlCon()
  u<-mutate(u,type=sapply(Lot,function(u){substr(u,1,1)}))
  un.u<-unique(u$type)
  if (length(un.u)==1){
      dbWriteTable(my_db, name=unname(platstring[un.u]),value=select(u,-type),
                   append=TRUE,overwrite = FALSE,row.names=FALSE)
    dbDisconnect(my_db)
  }else{
      dbWriteTable(my_db, name=unname(platstring[un.u[1]]),
                   value=select(filter(u,type== un.u[1]),-type),
                   append=TRUE,overwrite = FALSE,row.names=FALSE)
    dbDisconnect(my_db)
    UploadsCC(filter(u,type!= un.u[1]))
  }
}
