intermediateStep<-function(u){
   asyr::Collect( XML::xmlTreeParse(u,useInternalNodes =T))
}
