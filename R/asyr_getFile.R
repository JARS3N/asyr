getFile<-function(Z){basename(xpathSApply(Z,"//FileName",xmlValue))}
