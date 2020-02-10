
library('MASS');


setwd("");

test1<-read.csv("CandC-test1.csv",header = FALSE);
train1<-read.csv("CandC-train1.csv",header = FALSE);
#train-test2
test2<-read.csv("CandC-test2.csv",header = FALSE);
train2<-read.csv("CandC-train2.csv",header = FALSE);
#train-test3
test3<-read.csv("CandC-test3.csv",header = FALSE);
train3<-read.csv("CandC-train3.csv",header = FALSE);
#train-test4
test4<-read.csv("CandC-test4.csv",header = FALSE);
train4<-read.csv("CandC-train4.csv",header = FALSE);
#train-test5
test5<-read.csv("CandC-test5.csv",header = FALSE);
train5<-read.csv("CandC-train5.csv",header = FALSE);


lam=10000; 

# fit model
fit1 <- lm.ridge(train1$V123~., train1,lambda=lam);

# summarize the fit
summary(fit1)

# make predictions

abc=coef(fit1)[1] 
for(i in 2:length(coef(fit1)))
{
abc=abc+ coef(fit1)[i]*test1[,i-1];
}

rss1<-sqrt(sum((abc - test1$V123)^2));
sd1<-sd(abc-test1$V123);

#############residual error1 : 0.1383847###################################

# fit model
fit2 <- lm.ridge(train2$V123~., train2,lambda=lam)
# summarize the fit
summary(fit2)
# make predictions

abc=coef(fit2)[1] 
for(i in 2:length(coef(fit2)))
{
  abc=abc+ coef(fit2)[i]*test2[,i-1];
}

rss2<-sqrt(sum((abc - test2$V123)^2)/(nrow(test1)));
sd2<-sd(abc-test2$V123);

############residual error2 : 0.1334732###################################

# fit model
fit3 <- lm.ridge(train3$V123~., train3,lambda=lam)
# summarize the fit
summary(fit3)

# make predictions

abc=coef(fit3)[1] 
for(i in 2:length(coef(fit3)))
{
  abc=abc+ coef(fit3)[i]*test3[,i-1];
}

rss3<-sqrt(sum((abc - test3$V123)^2));
sd3<-sd(abc-test3$V123);

############residual error2 : 0.1601503###################################


# fit model
fit4 <- lm.ridge(train4$V123~., train4,lambda=lam)
# summarize the fit
summary(fit4)

# make predictions

abc=coef(fit4)[1] 
for(i in 2:length(coef(fit4)))
{
  abc=abc+ coef(fit1)[i]*test4[,i-1];
}

rss4<-sqrt(sum((abc - test4$V123)^2));
sd4<-sd(abc-test4$V123);


############residual error2 : 0.1531987###################################

# fit model
fit5 <- lm.ridge(train5$V123~., train5,lambda=lam)
# summarize the fit
summary(fit5)

# make predictions

abc=coef(fit5)[1] 
for(i in 2:length(coef(fit5)))
{
  abc=abc+ coef(fit5)[i]*test5[,i-1];
}

rss5<-sqrt(sum((abc - test5$V123)^2));
sd5<-sd(abc-test5$V123);

avgrss<-(rss1+rss2+rss3+rss4+rss5)/5;
############residual error2 : 0.1503388###################################

allCoeff<-rbind(fit1$coef,fit2$coef,fit3$coef,fit4$coef,fit5$coef);
write.table(as.numeric(fit1$coef),file = "CoeffRidge.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");

#####________________________________________________________________##########
##reduce features...
a<-sort.int(abs(fit2$coef), partial = NULL, na.last = NA, decreasing = TRUE,method = "shell", index.return = TRUE);
ind<-a$ix[1:122];
d_lab<-train1$V123
t_lab<-test1$V123
d<-train1[,ind]
t<-test1[,ind]
d<-cbind(d,d_lab)
t<-cbind(t,t_lab)
colnames(d)[123]<-"targ"
colnames(t)[123]<-"targ"
#permute test on the basis of indices


fit <- lm.ridge(t$targ~., t,lambda=lam);

# summarize the fit
summary(fit)

# make predictions

abc=coef(fit)[1] 
for(i in 2:length(coef(fit)))
{
  abc=abc+ coef(fit)[i]*t[,i-1];
}

rss<-sqrt(sum((abc - t$targ)^2));
sd<-sd(abc-t$targ);

#########even when chose 50 features rss remained same
