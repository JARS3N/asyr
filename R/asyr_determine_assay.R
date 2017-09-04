determine_assay<-function(file,template,force=F){
a<-unlist(sub_reg(tolower(template),"gain|ksv|barcode|wet qc xfp|wet qc|wetqc|mr"))
b<-unlist(sub_reg(tolower(file),"gain|ksv|barcode|wet qc xfp|wet qc|wetqc|mr"))
if(length(a)>0){return(a)}
if(length(b>0)){return(b)}else{
  return(NA)
}
}






