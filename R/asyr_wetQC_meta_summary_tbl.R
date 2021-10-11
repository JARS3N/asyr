wetQC_meta_summary_tbl <- function(lst) {
  df <- do.call('rbind', lapply(lst, asyr::wetQC_meta_summary))
  df[order(df$sn, method = "radix"), ]
}
