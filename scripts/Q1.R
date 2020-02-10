setwd("");

library('clusterGeneration');

##gen positive definite matrix
Cov<-genPositiveDefMat(20, covMethod="eigen",rangeVar=c(1,1.5), lambdaLow=1, ratioLambda=10);

##get diagonal elements of covariance matrix
dist<-diag(Cov$Sigma);

##alpha to be tunes by linear seperability check
alpha=.4; #setting it constant .. 

##gen means --------generating mean1 from some gaussion distribution only to see how both means vary

##give a random 20-d vector as prior mean
priorMean<-priorMean<-rep(c(1,0,2,0.5,4,0,3,2,0.5,1),2);
priorCov<-genPositiveDefMat(20, covMethod="eigen",rangeVar=c(1,1.1), lambdaLow=1, ratioLambda=10);

#using prior distribution, generate mean1
mean1<-mvrnorm(n = 1, priorMean, priorCov$Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE);

#mean2 as a function of mean1
mean2=mean1+alpha*dist;

#generate data
Class1<-mvrnorm(n = 2000, mean1, Cov$Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE);
Class2<-mvrnorm(n = 2000, mean2, Cov$Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE);

##check whether classes are linearly seperable or not and write data into csv files
#use checkLinearSeperabilty.R script

 
##Since data is generated randomly. we can subset data w/o sampling
Class1Test<-Class1[1:600, ];
Class2Test<-Class2[1:600, ];
Class1Train<-Class1[601:2000, ];
Class2Train<-Class1[601:2000, ];


#write it to file

write.table(Class1Test, file = "Class1Test.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(Class2Test, file = "Class2Test.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(Class1Train, file = "Class1Train.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(Class2Train, file = "Class2Train.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

