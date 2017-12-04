process_summary <- function(lst) {
  chk_lst <- function(u) {
    # col_assay <- c('ksv' = "KSV", 'gain' = 'Gain')[u$assay]
    # ck <- u[[u$assay]][[col_assay]]
    barcoded <- c('ksv' = u$O2_COEF$Ksv,
                  'gain' = u$PH_COEF$gain)
    median_gain = median(u[[u$assay]]$Gain)
    median_ksv = median(u[[u$assay]]$KSV)
    F0hat <- ((u$O2_COEF$Ksv * 152) + 1) * u$O2_COEF$target
    F0per00dif <- 100 * ((u[[u$assay]]$F0 - F0hat) / F0hat)
    inj <- if (u$assay == 'gain') {
      0
    } else{
      sum(abs(F0per00dif >= 10))####CHANGE BACK TO 10 ########
    }

    df <- data.frame(
      use = NA,
      Lot = u$Lot,
      sn = u$sn,
      assay = u$assay,
      pH.status = all(u$CAL$pH.Status == "Good"),
      o2.status = all(u$CAL$O2.Status == "Good"),
      Positive_Gain = median_gain > 0,
      Positive_KSV = median_ksv > 0,
      injections = (100 * (inj / length(
        u$CAL$O2.Status
      ))) <= 5,
      #assay.check = 100 * abs(mean(ck) - barcoded) / barcoded,
      file = u$file,
      stringsAsFactors = F
    )
    df$valid <- all(
      c(
        df$pH.status,
        df$o2.status,
        df$Positive_Gain,
        df$Positive_KSV,
        df$injections
      )
    )
    df

  }
  out <- lapply(lst, chk_lst)
  do.call('rbind', out)
}
