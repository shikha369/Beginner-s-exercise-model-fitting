##set working directory
setwd("");

library('stats');

##load from file
Class1Test<-read.csv("Class1Test.csv",header = FALSE);
Class2Test<-read.csv("Class2Test.csv",header = FALSE);
Class1Train<-read.csv("Class1Train.csv",header = FALSE);
Class2Train<-read.csv("Class2Train.csv",header = FALSE);


trainData<-rbind(Class1Train,Class2Train);
testData<-rbind(Class1Test,Class2Test);
##assign class labels
trainTarget<-c(rep(1,1400),rep(-1,1400));
testTarget<-c(rep(1,600),rep(-1,600));
##merge into data frame
train<-as.data.frame(cbind(trainData,trainTarget));
colnames(train)[21] <- "target";
test<-as.data.frame(cbind(testData,testTarget));
colnames(test)[21] <- "target";

##create a linear model
fit <- lm(train$target~.,train);
##predict on test data
predictions <- predict(fit, test);


##find fit precision and recall
##precision=tp/(tp+fp)
##recall=tp/(tp+fn)
index=c(1:1200)
tp1=0;fp1=0;fn1=0;
tp2=0;fp2=0;fn2=0;
for (i in index) {
  if(predictions[i]>=0&&test$target[i]>=0) tp1 = tp1+1;
  if(predictions[i]<0&&test$target[i]<0)  tp2 = tp2+1;
  if(predictions[i]<0&&test$target[i]>=0)  fp1 = fp1+1;
  if(predictions[i]>=0&&test$target[i]<0)  fp2 = fp2+1;
  if(predictions[i]>=0&&test$target[i]<0)  fn1 = fn1+1;
  if(predictions[i]<0&&test$target[i]>=0)  fn2 = fn2+1;
  
}
precision<-((tp1/(tp1+fp1))+(tp2/(tp2+fp2)))/2;
recall<-((tp1/(tp1+fn1))+(tp2/(tp2+fn2)))/2;
accuracy=recall; #it is same as recall
print("The fit precision is-----");print(precision);
print("The fit recall is-----");print(recall);
print("The fit accuracy is-----");print(accuracy);
##find fit F measure
##F=(2*precision*recall)/(precision+recall)
print("The fit F Measure is-----");
print((2*precision*recall)/(precision+recall));
##Coefficients Learned
write.table(as.numeric(fit$coefficients),file = "Coeff2.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");
