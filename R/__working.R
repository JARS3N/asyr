# library(XML)
#
# q<-list.files('sampledata',full.names=T)[1]
# w<-XML::xmlTreeParse(q,useInternalNodes =T)
# template<-xpathSApply(w,"//AssayTemplate",xmlValue)
# xp<-"sampledata/C08217_480_WETQC_040417_430029.asyr"
# w<-XML::xmlTreeParse(xp,useInternalNodes =T)
#
# fl<-xmlTreeParse("sampledata/C23517_60_WETQC_082517_430029.asyr" ,useInternalNodes = T)
#
#   xpathSApply(fl,path="//IsUserTemplate",xmlValue)=='true'
#
# xpathSApply(fl,path="//IsTemplate",xmlValue)
# basename(xpathSApply(fl,path="//AssayTemplate",xmlValue))
#
# fl<-xmlTreeParse("sampledata/072617_01_currRead_currBC_TypeB_MR.asyr" ,useInternalNodes = T)
#
#
# get_templateName<-function(X){
#   if(xpathSApply(fl,path="//IsUserTemplate",xmlValue)=='false'){
#     return(NA)
#   }else{
#     basename(xpathSApply(X,path="//AssayTemplate",xmlValue))
#   }
# }
# X<-XML::xmlTreeParse("sampledata/B14617.190.AE353.KSV.2017-08-02.asyr" ,useInternalNodes =T)



# lapply(x,function(x),unique(colnames(x)))
#
#
# names<-unique(unlist(lapply(list(a,b),function(x){unique(colnames(x))})))
#
#
#
#
#
# Q0<-lapply(fls,XML::xmlTreeParse)
# Q1<-lapply(Q0,PipeFish::Collect)
# Q2<-lapply(Q1,function(x){
#   y<-x$CAL
#   y$Lot<-x$Lot
#   y$sn<-x$sn
#   y$file<-x$file
#   y
# })
#
#library(XML)
#library(asyr)
#fls<-list.files('sampledata',full.names =T,pattern='asyr')
#Y0<-lapply(fls,XML::xmlTreeParse,useInternalNodes=T)




