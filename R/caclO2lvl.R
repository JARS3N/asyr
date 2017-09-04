#O2lvl <-(1/ksvcor)((f0/O2cem)-1)
#using
#O2lvl_notcorrected<-(1/ksvcor)*((f0/(O2light-O2dark))-1)


#ksv<-0.0209
#Target<-12500

#f0<-function(ksv,Target,mmHg=PipeFish::pO2(37)){
#  ((ksv*mmHg)+1)*Target
#}
#F0<-f0(ksv,Target)

#O2lvl<-function(O2cem,ksvcor,f0){
#  (1/ksvcor)*((f0/O2cem)-1)
#}


#KSV=((f0/target)-1)/152

# solve for f0

#f0 = ((ksv*mmHg)+1)*Target
