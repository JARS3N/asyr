test_if_zippped<-function(u){
# test if the file is just a stram or zip directory
  tryCatch({
    unzip(u, list = T)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}
