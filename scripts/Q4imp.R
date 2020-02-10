###Attribute Information: (122 predictive, 5 non-predictive, 1 goal)
##goal : ViolentCrimesPerPop
##data set alrady normalised

rawData<- read.delim("path/to/data",sep=",",na.strings=c("NA", "-", "?"),header=FALSE,stringsAsFactors=F);


##seperate predictive from non predictive attributes
rawData<-rawData[,6:128];

#rawData=data.matrix(rawData);
#####will be used to get mean: mean(age, na.rm = TRUE)
###Attribute Information: (122 predictive, 5 non-predictive, 1 goal)
##goal : ViolentCrimesPerPop
##data set already normalised
setwd("path/to/data");
rawData<- read.delim("communities.data",sep=",",na.strings=c("NA", "-", "?"),header=FALSE,stringsAsFactors=F);


##seperate predictive from non predictive attributes
rawData<-rawData[,6:128];

##imputation using missing values
c_new<-c(rep(0,1994))
for(i in 1:ncol(rawData))
{
  
  c_old<-rawData[i];
  c_old[is.na(c_old)] <- median(data.matrix(c_old), na.rm = TRUE);
  c_new<-cbind(c_new,c_old);
}
##write ImputedData into file
ImputedData<-c_new[,2:124];
Dataset<-cbind(rawData[,1:5],ImputedData);
write.table(ImputedData, file = "ImputedDatamedian.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");
write.table(Dataset, file = "ImputedDatasetmedian.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");

################################################################################
#or kNN in VIM
library("VIM")  
Data<-kNN(rawData)
write.table(Data, file = "ImputedDatakNN.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");
