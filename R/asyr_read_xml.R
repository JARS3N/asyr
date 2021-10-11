read_xml <- function(file) {
  if (test_if_zippped(file)) {
    name <- grep("ASSAYDATA", unzip(file, list = T)$Name, value = T)
    out <- XML::xmlTreeParse(unzip(file, name), useInternalNodes = T)
    unlink(name)
  } else{
    out <- XML::xmlTreeParse(x, useInternalNodes = T)
  }
  return(out)
}
