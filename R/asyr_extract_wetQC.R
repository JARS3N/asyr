extract_wetQC <- function (E) 
{
  output_name_order<-c("Well", "sorpH", "O2.LED", "O2.CalEmission", "O2.Status", 
                       "pH.LED", "pH.CalEmission", "pH.Status", "Target", "Gain", 
                       "Ambient", "F0", "KSV", "Inst", "sn", "Lot")
  if (E$assay== 'wetqc'){
    return(E$wetqc[output_name_order])
  }
  if (E$assay == "ksv") {
    E$ksv[c("Gain", "sorpH")] <- NA
  }
  if (E$assay == "gain") {
    E$gain[c("KSV", "F0", "Ambient")] <- NA
  }
#  E[[E$assay]][c("pH.LED", "pH.CalEmission", "pH.IntialReferenceDelta", 
#                 "pH.Status", "O2.LED", "O2.CalEmission", "O2.IntialReferenceDelta", 
#                 "O2.Status")] <- NULL
rm_nms <- c(
  "pH.LED",
  "pH.CalEmission",
  "pH.IntialReferenceDelta",
  "pH.Status",
  "O2.LED",
  "O2.CalEmission",
  "O2.IntialReferenceDelta",
  "O2.Status"
)

removing <- lapply(rm_nms, function(u) {
  E[[E$assay]][u] <<- NULL
}) 
  OUT <- merge(E$CAL, E[[E$assay]], by = "Well")
  OUT[c("Lot", "sn", "Inst", "Target")] <- list(E$Lot, E$sn, 
                                                E$Inst, E$PH_COEF$target)
  return(OUT[output_name_order])
}
