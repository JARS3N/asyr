wetQC_meta_summary <- function(u) {
  barcoded <- c(ksv = u$O2_coefs$Ksv,
                gain = u$pH_coefs$gain)
  check_median <-function(k){
    if (is.null(k)){
      1
    }else{
      median(k, na.rm = T)
    }
  }
  
  median_gain <- check_median(u$calibration$Gain)
  median_ksv  <- check_median(u$calibration$KSV)
  
  F0hat <- ((u$O2_coefs$Ksv * 151.69) + 1) * u$O2_coefs$target
  F0per00dif <- 100 * ((u$calibration$F0 - F0hat) / F0hat)
  inj <- if (u$assay == "gain") {
    0
  } else {
    sum(abs(F0per00dif >= 10))}

  df <-
    data.frame(
      use = NA,
      Lot = u$lot,
      sn = u$sn,
      assay=u$assay,
      pH.status = all(u$calibration$pH.Status == "Good"),
      o2.status = all(u$calibration$O2.Status =="Good"),
      Positive_Gain = median_gain > 0,
      Positive_KSV = median_ksv > 0,
      injections = (100 * (inj / length(u$calibration$O2.Status))) <=
        5,
      file = u$file,
      stringsAsFactors = F
    )
  df$valid <- all(c(
    df$pH.status,
    df$o2.status,
    df$Positive_Gain,
    df$Positive_KSV,
    df$injections
  ))
  df
}
