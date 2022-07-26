change_pH_coef <- function(file, PH_B = NULL, PH_C = NULL) {
  #c3 = ph_b,c4=ph_c
  change_val <- function(var, xpath) {
    node <- xpathApply(xml, xpath)
    sapply(node, function(N) {
      xmlValue(N) <- as.character(var)
    })
  }
  paths <- file.path(
  "",
  "",
  "Item[Key='pH']",
  "",
  "Value",
  "",
  "AnalyteCalibration",
  "",
  "GainEquation",
  "",
  c("C3", "C4"),
  "text()"
)
  
  xml <- XML::xmlTreeParse(file, useInternalNodes = T)
  
  Map(change_val, c(PH_B, PH_C), paths)
  
  name_xml <- gsub("asyr", "xml", file)
  name_asyr <- gsub("[.]asyr", "_corrected_5.asyr", file)
  XML::saveXML(xml, gsub("asyr", "xml", file))
  R.utils::gzip(name_xml, name_asyr)
}
