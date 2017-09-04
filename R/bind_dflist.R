bind_dflist<-function(V){
  names<-lapply(V,names)
  uniques<-unique(unlist(names))
  to_fill<-lapply(names,function(u){
    uniques[!uniques %in% u]
  })
  filledNAs<-Map(function(x,y){
    y[x]<-NA
    y
  },x=to_fill,y=V)
  do.call('rbind', filledNAs)
}
