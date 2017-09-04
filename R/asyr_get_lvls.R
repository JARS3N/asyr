get_lvls<-function(u,TickTable){

  minTick<-min(TickTable$Tick)
  pH_CorrectedEmission=xpathApply(u,"//Item[Key='pH']//Value//AnalyteDataSet//CorrectedEmissionValues",function(u){as.numeric(xmlSApply(u,xmlValue))})
  O2_CorrectedEmission=xpathApply(u,"//Item[Key='O2']//Value//AnalyteDataSet//CorrectedEmissionValues",function(u){as.numeric(xmlSApply(u,xmlValue))})
  timestamp<-xpathSApply(u,"//Item[Key='pH']//Value//AnalyteDataSet//TimeStamp",xmlValue)
  Out<- do.call('rbind',
    Map(function(x,y,z,a){
    Tick<-rep(z,length(x))
    data.frame(pH_CorrectedEmission=x,O2_CorrectedEmission=y,Tick,Well=seq_along(x),timestamp=a)
  },pH_CorrectedEmission,y=O2_CorrectedEmission,z=seq_along(pH_CorrectedEmission),a=timestamp
  ))

  minTick_Tick<-min(TickTable$Tick)
  minOut_Tick<-min(Out$Tick)
  if(minTick_Tick==0 && minOut_Tick==1 ){
    Out$Tick<-Out$Tick - 1
  }
  merge(Out,TickTable,by='Tick')
}


# microbenchmark(get_lvls(Xml),times=100,unit="s")
#
#
# xdx<-microbenchmark(
#   xpathApply(Xml,"//Item[Key='pH']//Value//AnalyteDataSet//CorrectedEmissionValues",function(u){as.numeric(xmlSApply(u,xmlValue))}),
#   as.numeric(xpathSApply(Xml,"//Item[Key='pH']//Value//AnalyteDataSet//CorrectedEmissionValues//double",xmlValue)),
#     xpathSApply(Xml,"//Item[Key='pH']//Value//AnalyteDataSet//CorrectedEmissionValues//double",function(u){as.numeric(xmlValue(u))}),
#     times=50,unit="s"
# )
##### the xpathSApply would be faster but need a way to still obtain the related Ticks

