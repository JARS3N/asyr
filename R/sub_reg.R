sub_reg<-function(str,match){
  regmatches(str,gregexpr(match,str))
}
