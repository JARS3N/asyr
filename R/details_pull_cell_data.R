pull_cell_data<-function(cell){
  cell<-cell[-which(names(cell)=='br')]
  b<-which(names(cell)=='b')
 # print(b)
  cell[(b[1]+1):(b[2]-2)]<-paste0("optical_",cell[(b[1]+ 1):(b[2]-2)] )
  cell[(b[2]+1):(b[3]-1)]<-gsub("center","spot_center",cell[(b[2]+1):(b[3]-1)])
  split_name_str<-function(str){
    splat<-strsplit(str,split=": ")
    setNames(
      lapply(splat,function(u){as.numeric(u[2])}),
      unlist(lapply(splat,function(u){gsub(" |-|, ","_",u[1])}))
    )
  }
  df<-as.data.frame(split_name_str(cell[-1*c(1,b)]))
  df$Well<-cell['center']
  df
}

