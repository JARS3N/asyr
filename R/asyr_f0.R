f0<-function(ksv,target,caltemp){
pp<-asyr::partial_pressure_ox(caltemp)
    ((ksv*pp)+1)*target
    }
