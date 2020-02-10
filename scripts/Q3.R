
##load data from file...

##set working directory
setwd("path/to/data");

library('stats');

##load from file
Class1Test<-read.csv("Class1Test.csv",header = FALSE);
Class2Test<-read.csv("Class2Test.csv",header = FALSE);
Class1Train<-read.csv("Class1Train.csv",header = FALSE);
Class2Train<-read.csv("Class2Train.csv",header = FALSE);

Class1<-rbind(Class1Train,Class1Test);
Class2<-rbind(Class2Train,Class2Test);

Data_n<-rbind(Class1,Class2);

target<-c(rep(1,2000),rep(-1,2000));
tData_n<-as.data.frame(cbind(Data_n,target));
train <- rbind(tData_n[1:1400,],tData_n[2001:3400,])
test <- rbind(tData_n[1401:2000,],tData_n[3401:4000,])
train_labels <- train[,21]
test_labels <- test[,21]

##NN classifier
library('class');
##Check with different value of k
test_pred <- knn(train =train, test =test,cl = train_labels, k=7);

##test its performance
library('gmodels');
performance=CrossTable(x=test_labels,y=test_pred,prop.chisq = FALSE);
##access all entries of table as : performance[1]$t[r,c]
##calculating performance measures
tp1=tn2=performance[1]$t[2,2];
tp2=tn1=performance[1]$t[1,1];
fp1=fn2=performance[1]$t[1,2];
fn1=fp2=performance[1]$t[2,1];

accuracy=(tp1+tp2)/(1200);
precision=((tp1/(tp1+fp1))+(tp2/(tp2+fp2)))/2;
recall=((tp1/(tp1+fn1))+(tp2/(tp2+fn2)))/2;
Fmeas=(2*precision*recall)/(precision+recall);
print('accuracy---');
print(accuracy);
print('precision---');
print(precision);
print('recall---');
print(recall);
print('Fmeasure---');
print(Fmeas);



