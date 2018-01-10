
UploadsCC<-function(u){
  require(RMySQL)
  platstring<-c(
    "C" ="xfpwetqc",
    "B" = "xfe24wetqc",
    "W" = "xfe96wetqc",
    "Q" = "xf24legacy"
  )

  if(length(u)<1){
    return(TRUE)
  }else{
    my_db <- adminKraken::con_mysql()
    dbWriteTable(my_db, 
                name=unname(platstring[substr(unique(u[[1]]$Lot),1,1)]),
                value=u[[1]],
                append=TRUE,
                overwrite = FALSE,
                row.names=FALSE)
    dbDisconnect(my_db)
    UploadsCC(u[-1])
  }
}
