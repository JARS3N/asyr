
CalibrationFailureModes<-function(B){
  require(dplyr);require(tidyr)
  data.frame(
    Low.O2LED=any(B$CAL$O2.LED<1000),
    Low.pHLED=any(B$CAL$pH.LED<1000),
    Low.O2RefDelta=any(B$CAL$O2.IntialReferenceDelta<1000),
    Low.pHRefDelta=any(B$CAL$pH.IntialReferenceDelta<1000),
    High.pHLED=any(B$CAL$pH.LED>(2*mean(B$CAL$pH.LED))),
    High.O2LED=any(B$CAL$O2.LED>(2*mean(B$CAL$O2.LED))),
    High.pHRef=any(B$CAL$pH.IntialReferenceDelta>(2*mean(B$CAL$pH.IntialReferenceDelta))),
    High.O2Ref=any(B$CAL$O2.IntialReferenceDelta>(2*mean(B$CAL$O2.IntialReferenceDelta))),
    TargetEm.pH=any(B$CAL$pH.CalEmission<(B$PH_COEF$target*.9))|any(B$CAL$pH.CalEmission>(B$PH_COEF$target*1.1)),
    TargetEm.O2=any(B$CAL$O2.CalEmission<(B$O2_COEF$target*.9))|any(B$CAL$O2.CalEmission>(B$O2_COEF$target*1.1)),
    Temp = B$calStartTemp <(36.8)| B$calStartTemp > (37.2)) %>%
    tidyr::gather(.,failureMode,Failed) %>%
    merge(.,data.frame(file=B$file))
}
getCalFailureModes<-function(DIR,parswitch=50){
  require(dplyr);
  FLS<- list.files(DIR,recursive=T,pattern='[.]asyr',full.names = T)
  if(length(FLS)<parswitch){
    FLS %>%
      lapply(., XML::xmlTreeParse,useInternalNodes = T) %>%
      lapply(.,PipeFish::CollectNoLVL) %>%
      lapply(.,CalibrationFailureModes) %>%
      bind_rows() %>%
      return()
  }else{
    require(parallel)
    size.of.list <- length(FLS)
    cl <- makeCluster( min(size.of.list, detectCores()) )
    OUT<-parallel::parLapply(cl=cl,FLS,function(u){XML::xmlTreeParse(u,useInternalNodes = T)}) %>%
      parallel::parLapply(cl=cl,.,PipeFish::CollectNoLVL) %>%
      parallel::parLapply(cl=cl,.,CalibrationFailureModes) %>%
      bind_rows()
    stopCluster(cl);
    return(OUT)
  }
}
