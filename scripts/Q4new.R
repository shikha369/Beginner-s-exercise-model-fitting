rawData<- read.delim("path/to/data",sep=",",na.strings=c("NA", "-", "?"),header=FALSE);
##generate NAs @ missing values

##seperate predictive from non predictive attributes
rawData<-rawData[,6:128];


cols_imputed <- lapply(rawData, function(col) {
  which_na <- which(is.na(col))
  num_na <- length(which_na)
  col <- replace(col, is.na(col), median(col, na.rm=TRUE))
  
  col
})


do.call(cbind.data.frame, cols_imputed)
