# This is to grab a variable AnalyteDataSet

get_raw_var<-function(var,xml,analyte){
  selected_analyte<-c("pH"="//Item[Key='pH']","O2"="//Item[Key='O2']")[analyte]
  center_str<-"Value//AnalyteDataSet"
  str_line<-paste(selected_analyte,center_str,var,sep="//")
  m_vecs<-xpathApply(xml,str_line,function(u){as.numeric(xmlSApply(u,xmlValue))})
  mapped<-Map(function(x,y){data.frame(var=x,Tick=y,Well=seq_along(x))},x=m_vecs,y=seq_along(m_vecs))
  setNames(Reduce('rbind',mapped),
           c(var,"Tick","Well")
  )
}
