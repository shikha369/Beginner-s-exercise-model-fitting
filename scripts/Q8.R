setwd("");
test = read.csv("test.csv",header = FALSE);
test_labels = read.csv("test_labels.csv",header = FALSE);
train = read.csv("train.csv",header = FALSE);
train_labels = read.csv("train_labels.csv",header = FALSE);
## no of classes = 2 --- labelled as 1 and 2 && no of features =3

#prepareset to plot
pt<-cbind(train,train_labels);
colnames(pt)[4] <- "target";

#perform lda on trained data 
tr.lda <- lda(pt$target~.,data=pt); 
print(tr.lda);


# Predict on test data
pred_lda<-predict(object = tr.lda, newdata = test);
predictions<-pred_lda$x;

##find fit accuracy
index=c(1:400)
count=0;
for (i in index) {
  if((predictions[i]<1.5&&test_labels[i,1]==1)||(predictions[i]>=1.5&&test_labels[i,1]==2))  count = count+1
}
print("The Best fit Accuracy is-----");print(count/400);


##find fit precision and recall
##precision=tp/(tp+fp)
##recall=tp/(tp+fn)
index=c(1:400)
tp1=0;fp1=0;fn1=0;
tp2=0;fp2=0;fn2=0;
for (i in index) {
  if(predictions[i]<1.5&&test_labels[i,1]==1) tp1 = tp1 + 1;
  if(predictions[i]>=1.5&&test_labels[i,1]==2)  tp2 = tp2+1;
  if(predictions[i]<1.5&&test_labels[i,1]==2)  fp1 = fp1+1;
  if(predictions[i]>=1.5&&test_labels[i,1]==1)  fp2 = fp2+1;
  if(predictions[i]>=1.5&&test_labels[i,1]==1)  fn1 = fn1+1;
  if(predictions[i]<1.5&&test_labels[i,1]==2)  fn2 = fn2+1;
  
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


###_______________________________________________PLOTS___________________________________________________
library(plot3D)

pt1<-pt[pt$target==1,]
pt2<-pt[pt$target==2,]

##take projections on lda coeff.
pred_lda1<-predict(object = tr.lda, newdata = pt1);
predictions1<-pred_lda1$x;

pred_lda2<-predict(object = tr.lda, newdata = pt2);
predictions2<-pred_lda2$x;

#simply plot x coords
x <- list("Class1"=predictions1, "Class2"=predictions2,"test"=predictions)
stripchart(x,
           main="Projection of data using LDA",
           xlab="direction",
           ylab="Classes",
           method="jitter",
           col=c("orange","red","blue"),
           pch=16
)





