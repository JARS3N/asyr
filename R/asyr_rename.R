rename<-function(FILE,newname,remove=FALSE){
XM<-XML::xmlParse(FILE)
node <- xpathApply(XM, path="//file//text()")
sapply(node, function(H) {xmlValue(H) <- newname})
newxml<-gsub("asyr$","xml",newname)
XML::saveXML(XM, newxml)
R.utils::gzip(newxml,newname) 
if(remove==FALSE){unlink(FILE)}
}
