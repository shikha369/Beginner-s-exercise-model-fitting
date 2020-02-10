
#No of neurons in input layer(no of features) : In
#No of neurons in hidden layer                : Hn
#No of neurons in output layer(no of classes) : On 
#Weight matrix between Input and Hidden Layer : Wih[In][Hn]
#bias input to hidden                         : b1[Hn]
#Weight matrix between Hidden and Output Layer: Who[Hn][On]
#bias hidden to output                        : b2[On]
#Learning rate : alpha
#Activation@input Layer is same as InputtoInputLayer
#Activation@Hidden Layer is same as Summation of weighted Inputs to that hidden node
#Function@Hidden Layer : Sigmoid()
#Activation@Output Layer is same as Summation of weighted Inputs to that output node
#Function@Output Layer : SoftMax() that takes into account all the outputs from the output Layer



#_____________________________________________________________________________________________________
##change to data directory

setwd("");

##Load Data
library('e1071');
library('caret');
TrainM<- read.csv("Trainmountain.csv",sep=",",header=FALSE);
TrainI<- read.csv("Traininsidecity.csv",sep=",",header=FALSE);
TrainF<- read.csv("Trainforest.csv",sep=",",header=FALSE);
TrainC<- read.csv("Traincoast.csv",sep=",",header=FALSE);
TestM<- read.csv("Testmountain.csv",sep=",",header=FALSE);
TestI<- read.csv("Testinsidecity.csv",sep=",",header=FALSE);
TestF<- read.csv("Testforest.csv",sep=",",header=FALSE);
TestC<- read.csv("Testcoast.csv",sep=",",header=FALSE);

##create target labels

Data<-rbind(TrainM,TrainI,TrainF,TrainC);
TrainLabel<-as.factor(c(rep('M',nrow(TrainM)),rep('I',nrow(TrainI)),rep('F',nrow(TrainF)),rep('C',nrow(TrainC))));
Test<-rbind(TestM,TestI,TestF,TestC);
TestLabel<-as.factor(c(rep('M',nrow(TestM)),rep('I',nrow(TestI)),rep('F',nrow(TestF)),rep('C',nrow(TestC))));

##Preprocess Data_________________________________________________________________________________________

library('caret');
p<-preProcess(Data, method = "range");
trainTransformed <- as.matrix(predict(p, Data));
testTransformed <- as.matrix(predict(p, Test));
#trainTransformed <- log(trainTransformed)
pc=prcomp(trainTransformed,center = TRUE,scale. = TRUE); 
trainTransformed=pc$x;
testTransformed=predict(pc,testTransformed);
##Create output vectors_________________________________________________________________________________

Trainoutput<-matrix(0,nrow=nrow(Data),ncol=4);
Testoutput<-matrix(0,nrow=nrow(Test),ncol=4);
#Sequence(MIFC)
for(i in 1:length(TrainLabel))
{
  if(TrainLabel[i]=='M')
    Trainoutput[i,1]=1;
  if(TrainLabel[i]=='I')
    Trainoutput[i,2]=1;
  if(TrainLabel[i]=='F')
    Trainoutput[i,3]=1;
  if(TrainLabel[i]=='C')
    Trainoutput[i,4]=1;
}


for(i in 1:length(TestLabel))
{
  if(TestLabel[i]=='M')
    Testoutput[i,1]=1;
  if(TestLabel[i]=='I')
    Testoutput[i,2]=1;
  if(TestLabel[i]=='F')
    Testoutput[i,3]=1;
  if(TestLabel[i]=='C')
    Testoutput[i,4]=1;
}
##___________________________________________________________________________________________________________

##Initialise all weights to random numbers b/w -0.2 to 0.2
In<-ncol(Data);
Hn<-5; ##setting no oh hidden nodes
On<-4;##Number of classes
W1<-matrix(runif((In)*Hn,-0.2,0.2),nrow=In,ncol=Hn);
W2<-matrix(runif((Hn)*On,-0.2,0.2),nrow=Hn,ncol=On);

alpha=0.2;  ##vary to check performance
reg_lambda1=.01; ##vary to check performance
reg_lambda2=10; ##vary to check performance
#b1=matrix(0,nrow=nrow(Data),ncol=1);
#b2=matrix(0,nrow=Hn,ncol=1);
b1=matrix(0,nrow=1,ncol=Hn);
b2=matrix(0,nrow=1,ncol=On);
X=trainTransformed;

##_____________________________________________________________________________________________________
nIter=1000;
for (i in 1:nIter)
{
  print(i);
  # Forward propagation
  z1 =  X%*%W1+  matrix(rep(b1,1006), nrow = 1006, byrow = T);
  a1 = sigmoid(z1);
  z2 = a1%*%W2 + matrix(rep(b2,1006), nrow = 1006, byrow = T);;
  exp_scores = exp(z2);
  probs = exp_scores / rowSums(exp_scores);
  
  # Backpropagation
  delta3 = probs;
  delta3=delta3-Trainoutput;
  dW2 = t(a1)%*%delta3;
  db2 = colSums(delta3);
  #db2 = delta3;
  delta2 = (delta3%*%t(W2))*(a1 -a1*a1);
  dW1 = t(X)%*%delta2;
  db1 = colSums(delta2);
  
  #db1 = delta2;
  #Add regularization terms (b1 and b2 don't have regularization terms)
  ##_____________CoMMent_________________________for orig BPNN___________
  dW2 = dW2 + reg_lambda2 * W2
  dW1 = dW1 +reg_lambda1 * W1
  ##_____________till here_________________________for orig BPNN___________
  
  
  
  # Gradient descent parameter update
  W1 = W1 -alpha * dW1;
  b1 = b1 -alpha * db1;
  W2 = W2 -alpha * dW2;
  b2 = b2 -alpha * db2;
  
}



#______________________________________________________________________________________________________

##Testing
XX = testTransformed;
z1 = XX%*%W1 + matrix(rep(b1,80), nrow = 80, byrow = T);
a1 = sigmoid(z1);
z2 = a1%*%W2 + matrix(rep(b2,80), nrow = 80, byrow = T);
exp_scores = exp(z2);
probs = exp_scores / rowSums(exp_scores);


predicted_class <- apply(probs, 1, which.max);
true_class<-apply(Testoutput, 1, which.max);
c_matrix=confusionMatrix(predicted_class,true_class);
print(c_matrix);




