upload_process_summary<-function(u){
  require(RMySQL)
  my_db <- adminKraken::con_mysql()
  platstring<-c(
    "C" ="xfpwetqc",
    "B" = "xfe24wetqc",
    "W" = "xfe96wetqc",
    "Q" = "xf24legacy"
  )
 
  types<-sapply(u$Lot,function(u){substr(u,1,1)})
  u$table <- platstring[types]
  u$user <- Sys.getenv('USERNAME')
  u$date <- Sys.time()
  RMySQL::dbWriteTable(my_db,
               name='wetqctransactions',
               value=u,
               overwrite=F,
               append=T
  )

}
