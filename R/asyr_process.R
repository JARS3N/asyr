process<-function(X){
  require(XML)
  require(asyr)
  #"gain|ksv|barcode|wet qc xfp|mr" - allowed assays
  E<-list(
    file =asyr::getFile(X))
  E$SW = asyr::swversion(X)
  E$PH_COEF = asyr::pH_coefs(X)
  E$O2_COEF = asyr::O2_coefs(X)
  E$Inst = asyr::getInst(X)
  E$sn = asyr::getSn(X)
  E$Lot = asyr::getLot(X)
  E$template=asyr::get_template_name(X)
  E$assay = asyr::determine_assay(E$file,E$template)
  E$TickTable<-asyr::tick_table(X)
  E$CAL <- asyr::CalData(X)
  E$LVL <- asyr::get_lvls(X,E$TickTable)
  puke<-function(){TRUE}
  Flist<-list(
    'gain'=newGain,
    'ksv'=Ksv,
    'wet qc xfp'=ComboAssay,
    'wet_qc'=ComboAssay,
    'wetqc'=ComboAssay,
    'mr'=puke,
    'barcode'=puke
  )
    E[[E$assay]]<-Flist[[E$assay]](E)
    E
}
