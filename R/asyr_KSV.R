Ksv<-function(X){
  spl<-lapply(split(X$LVL,X$LVL$Measure),function(u){
   df<- u[u$Tick %in% (max(u$Tick)-c(2,1,0)),c('Tick','O2_CorrectedEmission','Well','Measure')]
   agg<-aggregate(O2_CorrectedEmission~Well,data=df,FUN="mean")
   m<-c('Ambient','F0')[unique(df$Measure)]
   names(agg)<-c('Well',m)
   agg
  })
  comb<-Reduce('merge',spl)[c('Well','Ambient','F0')]
  comb$KSV <-(((comb$F0/comb$Ambient)-1)/152)
merge(comb,X$CAL,by='Well')
}

