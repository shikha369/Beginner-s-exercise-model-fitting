setwd("");
test = read.csv("test.csv",header = FALSE);
test_labels = read.csv("test_labels.csv",header = FALSE);
train = read.csv("train.csv",header = FALSE);
train_labels = read.csv("train_labels.csv",header = FALSE);
## no of classes = 2 --- labelled as 1 and 2 & no of features =3

#prepareset to plot
pt<-cbind(train,train_labels);
colnames(pt)[4] <- "target";

#perform pca on trained data 
tr.pca <- prcomp(train,center = TRUE,scale. = TRUE); 
print(tr.pca);
#PC are eigen vector
plot(tr.pca, type = "l");

transformedData<-tr.pca$x;

# Predict PCs on test data
test_transformed<-predict(tr.pca,newdata=test)

#choose most significant direction
tr_data<-transformedData[,1:3];
ts_data<-test_transformed[,1:3];

#use the data in this projected space to train linear regression with indicator random variables
tr_data<-cbind(tr_data,train_labels);
ts_data<-cbind(ts_data,test_labels);


colnames(tr_data)[1] <- "feature";
colnames(ts_data)[1] <- "feature";
colnames(tr_data)[4] <- "target";
colnames(ts_data)[4] <- "target";


##create a linear model
fit <- lm(tr_data$target~.,tr_data);
predictions <- predict(fit, ts_data);

##find fit accuracy
index=c(1:400)
count=0;
for (i in index) {
  if((predictions[i]<1.5&&ts_data$target[i]==1)||(predictions[i]>=1.5&&ts_data$target[i]==2))  count = count+1
}
print("The Best fit Accuracy is-----");print(count/400);


##find fit precision and recall
##precision=tp/(tp+fp)
##recall=tp/(tp+fn)
index=c(1:400)
tp1=0;fp1=0;fn1=0;
tp2=0;fp2=0;fn2=0;
for (i in index) {
  if(predictions[i]<1.5&&ts_data$target[i]==1) tp1 = tp1 + 1;
  if(predictions[i]>=1.5&&ts_data$target[i]==2)  tp2 = tp2+1;
  if(predictions[i]<1.5&&ts_data$target[i]==2)  fp1 = fp1+1;
  if(predictions[i]>=1.5&&ts_data$target[i]==1)  fp2 = fp2+1;
  if(predictions[i]>=1.5&&ts_data$target[i]==1)  fn1 = fn1+1;
  if(predictions[i]<1.5&&ts_data$target[i]==2)  fn2 = fn2+1;
  
}
precision1<-(tp1/(tp1+fp1));
precision2<-(tp2/(tp2+fp2));
recall1<-(tp1/(tp1+fn1));
recall2<-(tp2/(tp2+fn2));
print("The class1 precision is-----");print(precision1);
print("The class2 precision is-----");print(precision2);
print("The class1 recall is-----");print(recall1);
print("The class2 recall is-----");print(recall2);
##find fit F measure
##F=(2*precision*recall)/(precision+recall)
print("The class1 F Measure is-----");
print((2*precision1*recall1)/(precision1+recall1));
print("The class2 F Measure is-----");
print((2*precision2*recall2)/(precision2+recall2));

##Coefficients Learned
print(fit$coefficients);

###_______________________________________________PLOTS___________________________________________________
library(plot3D)
##plot data in original space
pt1<-pt[pt$target==1,]
pt2<-pt[pt$target==2,]
a<-pt1[,1];
b<-pt1[,2];
c<-pt1[,3];
d<-pt2[,1];
e<-pt2[,2];
f<-pt2[,3];
scatter3D(a, b, c,bty = "g",pch = 20, cex = 1)
scatter3D(d, e, f, add = TRUE, colkey = FALSE, pch = 21, cex = 1)
##plot in projected space
ptt<-cbind(transformedData,train_labels);
colnames(ptt)[4] <- "target";
pt1<-ptt[ptt$target==1,1:3]
pt2<-ptt[ptt$target==2,1:3]
a<-pt1[,1];
b<-pt1[,2];
c<-pt1[,3];
d<-pt2[,1];
e<-pt2[,2];
f<-pt2[,3];

plot<-scatter3D(a, b, c,bty = "g",pch = 20, cex = 1)
plot<-scatter3D(d, e, f, add = TRUE, colkey = FALSE, pch = 21, cex = 1)






