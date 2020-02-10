
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


# fit model
fit1 <- lm(train1$V123~., train1)
# summarize the fit
summary(fit1)
# make predictions
predictions1 <- predict(fit1, test1)
rss1<-sqrt(sum((predictions1-test1$V123)^2));
sd1<-sd(predictions1-test1$V123);
#############residual error1 : 0.1383847###################################

# fit model
fit2 <- lm(train2$V123~., train2)
# summarize the fit
summary(fit2)
# make predictions
predictions2 <- predict(fit2, test2)
rss2<-sqrt(sum((predictions2-test2$V123)^2));
sd2<-sd(predictions2-test2$V123);
############residual error2 : 0.1334732###################################

# fit model
fit3 <- lm(train3$V123~., train3)
# summarize the fit
summary(fit3)
# make predictions
predictions3 <- predict(fit3, test3)
rss3<-sqrt(sum((predictions3-test3$V123)^2));
sd3<-sd(predictions3-test3$V123);
############residual error2 : 0.1601503###################################


# fit model
fit4 <- lm(train4$V123~., train4)
# summarize the fit
summary(fit4)
# make predictions
predictions4 <- predict(fit4, test4)
rss4<-sqrt(sum((predictions4-test4$V123)^2));
sd4<-sd(predictions4-test4$V123);

############residual error2 : 0.1531987###################################

# fit model
fit5 <- lm(train5$V123~., train5)
# summarize the fit
summary(fit5)
# make predictions
predictions5 <- predict(fit5, test5)
rss5<-sqrt(sum((predictions5-test5$V123)^2));
sd5<-sd(predictions5-test5$V123);
############residual error2 : 0.1503388###################################

avgrss<-(rss1+rss2+rss3+rss4+rss5)/5;

allCoeff<-rbind(fit1$coefficients,fit2$coefficients,fit3$coefficients,fit4$coefficients,fit5$coefficients);
write.table(as.numeric(allCoeff),file = "Coeff5kNN.csv",row.names=FALSE, na="",col.names=FALSE, sep=",");


