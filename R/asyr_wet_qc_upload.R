wet_qc_upload <- function(df) {
  require(RMySQL)
  platstring <-
    c(C = "xfpwetqc",
      B = "xfe24wetqc",
      W = "xfe96wetqc",
      Q = "xf24legacy")
  my_db <- adminKraken::con_mysql()
  dbWriteTable(
    my_db,
    name = unname(platstring[substr(unique(df$Lot),
                                    1, 1)]),
    value = df ,
    append = TRUE,
    overwrite = FALSE,
    row.names = FALSE
  )
  dbDisconnect(my_db)
}
