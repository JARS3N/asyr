oldGain<-function(X){

    mutate(.,Target=X$PH_COEF$target) %>%
    mutate(.,Gain=(Target/m1)*(1/800)*(m1-sorpH)) %>%
    select(.,-m1) %>%
    arrange(.,Well)
}
old_gain<-function(X){
meas<-lapply(
  split(
    X$LVL[X$LVL$Measure<3,c("Tick","pH_CorrectedEmission","Well","Measure")],
    X$LVL$Measure
  ),
  function(u){
    setNames(aggregate(pH_CorrectedEmission~Well+Measure,
            u[u$Tick>(max(u$Tick)-2),]
            ,mean)[,c("Well","CorrectedEmission")],
  c("Well",paste0("m",unique(u$Measure))))
            }
  )

OUT<-merge(meas[[1]],meas[[2]],by='Well')
OUT[c("Target","Gain")]<-list(X$PH_COEF$target,
                              (X$PH_COEF$target/OUT$m1)*(1/800)*(OUT$m1-OUT$m2)
                              )
setNames(
  OUT[,c("Well","m2","Target","Gain")],
  c("Well","sorpH","Target","Gain"))
}
