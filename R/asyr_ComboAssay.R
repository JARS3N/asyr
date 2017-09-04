# stil may be some speed improvemets to be made
#X<-asyr::Collect(w)
ComboAssay<-function(X){
pLVL<-X$LVL[X$LVL$Measure==1,c("Well","Tick","Measure","pH_CorrectedEmission")]
tick<-pLVL$Tick
filtered_lvls<-tick %in% (max(tick)-c(2,1,0))

pHdf<-merge(
  setNames(aggregate(pH_CorrectedEmission~Well,data=pLVL[filtered_lvls,],mean),c("Well","sorpH")),
  X$CAL,by='Well'
)
pHdf[c("Target","Gain")]<-list(
  X$PH_COEF$target,
  (X$PH_COEF$target/pHdf$pH.CalEmission)*(1/800)*(pHdf$pH.CalEmission-pHdf$sorpH)
)
O2df<-X$LVL[X$LVL$Measure==1 | X$LVL$Measure ==2 ,
            c("Well","O2_CorrectedEmission","Tick","Measure")]

O2df$Measure<-c("Ambient","F0")[O2df$Measure]
kcalc<-lapply(split(O2df,O2df$Measure),
            function(u){
              t<-u$Tick
    out<- aggregate(O2_CorrectedEmission~Well,
               data=u[t %in% (max(t)-c(2,1,0)),],FUN="mean")
    setNames(out,c("Well",unique(u$Measure)))
            })

ksv<- merge(kcalc$Ambient,kcalc$F0,by='Well')
ksv$ksv<-((ksv$F0/ksv$Ambient)-1)/152


combo<-merge(pHdf,ksv,by='Well')
combo[c("Inst","Lot","sn")]<-list(X$Inst,X$Lot,X$sn)
combo
}

