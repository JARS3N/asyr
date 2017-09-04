Collect<-function(X){
  list(
    CAL = CalData(X),
    LVL = get_lvls(X),
    PH_COEF=pH_coefs(X),
    O2_COEF=O2_coefs(X),
    Inst = getInst(X),
    sn = getSn(X),
    Lot = getLot(X),
    assay=determineAssay(X),
    file=getFile(X),
    SW=swversion(X)
  )
}
CollectNoLVL<-function(X){
  list(
    CAL = CalData(X),
    calStartTemp=calStartTemp(X),
    PH_COEF=pH_coefs(X),
    O2_COEF=O2_coefs(X),
    Inst = getInst(X),
    sn = getSn(X),
    Lot = getLot(X),
    assay=determineAssay(X),
    file=getFile(X),
    SW=swversion(X)
  )
}
