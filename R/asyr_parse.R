parse<-function(X){
  require(XML)
  require(asyr)
  E<-list(
    file = asyr::getFile(X))
  E$SW = asyr::swversion(X)
  E$PH_COEF = asyr::pH_coefs(X)
  E$O2_COEF = asyr::O2_coefs(X)
  E$Inst = asyr::getInst(X)
  E$sn = asyr::getSn(X)
  E$Lot = asyr::getLot(X)
  E$TickTable<-asyr::tick_table(X)
  E$CAL <- asyr::CalData(X)
  E$LVL <- asyr::get_lvls(X,E$TickTable)
  E$cal_temp <- asyr::cal_temp(X)
  E$O2_COEF$F0<-asyr::f0(E$O2_COEF$Ksv,E$O2_COEF$target,E$cal_temp)
  E$LVL$mmHgO2<-(1/E$O2_COEF$Ksv) * ((F0(E$O2_COEF$Ksv,E$O2_COEF$target)/E$LVL$O2_CorrectedEmission)-1)
  E$LVL$file<-E$file
  E
}
