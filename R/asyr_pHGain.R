newGain<-function(X){
  tick<-X$LVL$Tick
 filtered_lvls<-tick %in% (max(tick)-c(2,1,0))
 lvl_agg<-aggregate(pH_CorrectedEmission~Well,data=X$LVL[filtered_lvls,],FUN=mean)
 U<-merge(lvl_agg,X$CAL,by="Well")
  LVL<-setNames(U,gsub("pH_CorrectedEmission","sorpH",names(U)))
  LVL$Target<- X$PH_COEF$target
  LVL$Gain<-(LVL$Target/LVL$pH.CalEmission)*(1/800)*(LVL$pH.CalEmission-LVL$sorpH)
  LVL
}

