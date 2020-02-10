#Logistic regression

setwd("");
test_Forest = read.csv("testforest.csv",header = FALSE);
test_Mountain= read.csv("testmountain.csv",header = FALSE);
train_Forest = read.csv("trainforest.csv",header = FALSE);
train_Mountain = read.csv("trainmountain.csv",header = FALSE);

train_Forest<-cbind(train_Forest,as.numeric(rep(1,nrow(train_Forest))));
train_Mountain<-cbind(train_Mountain,as.numeric(rep(0,nrow(train_Mountain))));

colnames(train_Forest)[ncol(train_Forest)]<-"target"
colnames(train_Mountain)[ncol(train_Mountain)]<-"target"

trainSet<-rbind(train_Forest,train_Mountain);

#####fitting logistic model
model <- glm(trainSet$target ~.,family=binomial(link='logit'),data=trainSet);

#testing model on Forest test data
fitted.results1 <- predict(model,newdata=test_Forest,type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.5,1,0)
misClasificError <- mean(fitted.results1 != 1)  #target=1 for forest
print(paste('Accuracy',1-misClasificError))

#testing model on Mountain test data
fitted.results0 <- predict(model,newdata=test_Mountain,type='response')
fitted.results0 <- ifelse(fitted.results0 < 0.5,0,1)
misClasificError <- mean(fitted.results0 != 0)  #target=0 for Mountain
print(paste('Accuracy',1-misClasificError))

#########________________________________Performance___________________________
## getting labels as column vector ;)
fitted.results1<-t(fitted.results1)
fitted.results1<-t(fitted.results1)
fitted.results0<-t(fitted.results0)
fitted.results0<-t(fitted.results0)

fitted.results<-as.numeric(rbind(fitted.results1,fitted.results0));
true<-rbind(t(t(as.numeric(rep(1,nrow(test_Forest))))),t(t(as.numeric(rep(0,nrow(test_Mountain))))));

correct0=0;correct1=0;
for(i in 1:nrow(true))
{
  if(fitted.results[i]==0&&true[i]==0) correct0=correct0+1;
  if(fitted.results[i]==1&&true[i]==1) correct1=correct1+1;
}
precision1=correct1/length(fitted.results[fitted.results==1]); #correct/retrieved
recall1=correct1/length(true[true==1]); #correct/true
fm1=2 * precision1 * recall1 / (precision1 + recall1);
  
precision0=correct0/length(fitted.results[fitted.results==0]); #correct/retrieved
recall0=correct0/length(true[true==0]); #correct/true
fm0=2 * precision0 * recall0 / (precision0 + recall0);

###_______________________L1 Regularised__________________________________________
library(LiblineaR);

L1_Reg_model<-LiblineaR(trainSet, trainSet$target, type = 6, cost = 0.05, epsilon = 0.01);
predictions.L1.F<-predict(L1_Reg_model,test_Forest);
predictedF<-predictions.L1.F$predictions;
predictions.L1.M<-predict(L1_Reg_model,test_Mountain);
predictedM<-predictions.L1.M$predictions;
predictedM<-t(t(predictedM));
predictedF<-t(t(predictedF));
precdictedL<-(rbind(predictedF,predictedM));
true<-rbind(t(t(as.numeric(rep(1,nrow(test_Forest))))),t(t(as.numeric(rep(0,nrow(test_Mountain))))));


correctL0=0;correctL1=0;
for(i in 1:nrow(true))
{
  if(precdictedL[i]==0&&true[i]==0) correctL0=correctL0+1;
  if(precdictedL[i]==1&&true[i]==1) correctL1=correctL1+1;
}
precisionL1=correctL1/length(precdictedL[precdictedL==1]); #correct/retrieved
recallL1=correctL1/length(true[true==1]); #correct/true
fmL1=2 * precisionL1 * recallL1 / (precisionL1 + recallL1);

precisionL0=correctL0/length(precdictedL[precdictedL==0]); #correct/retrieved
recallL0=correctL0/length(true[true==0]); #correct/true
fmL0=2 * precisionL0 * recallL0 / (precisionL0 + recallL0);


##********CLASS FOREST HAS BETTER ACCURACY THAN MOUNTAIN ******************* 

