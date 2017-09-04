get_template_name<-function(X){
  if(xpathSApply(X,path="//IsUserTemplate",xmlValue)=='false'){
    return(NA)
  }else{
    basename(xpathSApply(X,path="//AssayTemplate",xmlValue))
  }
}
