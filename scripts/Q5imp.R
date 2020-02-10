#load imputed data
Data<-ImputedData;

#Shuffle Dataset to make 80-20 splits
D <- Data[sample(nrow(Data)),];

#train-test1
test1<-D[1:300,];
train1<-D[301:1508,];
#train-test2
test2<-D[301:600,];
train2<-rbind(D[1:300,],D[601:1508,]);
#train-test3
test3<-D[601:900,];
train3<-rbind(D[1:600,],D[901:1508,]);
#train-test4
test4<-D[901:1200,];
train4<-rbind(D[1:900,],D[1201:1508,]);
#train-test5
test5<-D[1201:1500,];
train5<-rbind(D[1:1200,],D[1501:1508,]);

##write all in csvs

# fit model
fit1 <- lm(train1$V128~., train1)
# summarize the fit
summary(fit1)
# make predictions
predictions1 <- predict(fit1, test1)
rss1<-sqrt(sum((predictions1-test1$V128)^2)/(nrow(test1)-2));
sd1<-sd(predictions1-test1$V128);
#############residual error1 : 0.1383847###################################

# fit model
fit2 <- lm(train2$V128~., train2)
# summarize the fit
summary(fit2)
# make predictions
predictions2 <- predict(fit2, test2)
rss2<-sqrt(sum((predictions2-test2$V128)^2)/(nrow(test2)-2));
sd2<-sd(predictions2-test2$V128);
############residual error2 : 0.1334732###################################

# fit model
fit3 <- lm(train3$V128~., train3)
# summarize the fit
summary(fit3)
# make predictions
predictions3 <- predict(fit3, test3)
rss3<-sqrt(sum((predictions3-test3$V128)^2)/(nrow(test3)-2));
sd3<-sd(predictions3-test3$V128);
############residual error2 : 0.1601503###################################


# fit model
fit4 <- lm(train4$V128~., train4)
# summarize the fit
summary(fit4)
# make predictions
predictions4 <- predict(fit4, test4)
rss4<-sqrt(sum((predictions4-test4$V128)^2)/(nrow(test4)-2));
sd4<-sd(predictions4-test4$V128);

############residual error2 : 0.1531987###################################

# fit model
fit5 <- lm(train5$V128~., train5)
# summarize the fit
summary(fit5)
# make predictions
predictions5 <- predict(fit5, test5)
rss5<-sqrt(sum((predictions5-test5$V128)^2)/(nrow(test5)-2));
sd4<-sd(predictions5-test5$V128);
############residual error2 : 0.1503388###################################





