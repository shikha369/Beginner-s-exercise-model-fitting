###Attribute Information: (122 predictive, 5 non-predictive, 1 goal)
##goal : ViolentCrimesPerPop
##data set already normalised
setwd("path/to/data");
rawData<- read.delim("communities.data",sep=",",na.strings=c("NA", "-", "?"),header=FALSE,stringsAsFactors=F);


##seperate predictive from non predictive attributes
rawData<-rawData[,6:128];

#_____________________________________________________________________________________________________________

# find out how much data is missing------------
library('VIM');
a<-aggr(rawData);
print(a);
#_____________________________________________________________________________________________________________
##imputation using Mean values
c_new<-c(rep(0,1994))
for(i in 1:ncol(rawData))
{

  c_old<-rawData[i];
  c_old[is.na(c_old)] <- mean(data.matrix(c_old), na.rm = TRUE);
  c_new<-cbind(c_new,c_old);
}
##write ImputedData into file
ImputedData<-c_new[,2:124];
Dataset<-cbind(rawData[,1:5],ImputedData);
write.table(ImputedData, file = "ImputedDatamean.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");
write.table(Dataset, file = "ImputedDatasetmean.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");
