set.seed(26)
library(gbm)

#<---------Data Loading----------------->
#Master_Test & Train
train_master <- read.csv("Yes_Bank_Train.csv")
test_master <- read.csv("Yes_Bank_Test_int.csv")

#Test  & Train from master
cutoff = round(0.7*nrow(train_master)) #70:30 split for train and test respectively
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]

#Gradient Boosting Model
set.seed(26)
gbm_1 <- gbm(credit_amount ~., data = train, cv.folds = 4,distribution = "gaussian", n.trees = 18000, shrinkage = 0.001, interaction.depth = 5)
plot(gbm_1, i="duration_month")
plot(gbm_1, i="purpose")
n.trees = seq(from=100 ,to=18000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm_1,train,n.trees = n.trees)
dim(predmatrix)
test.error<-with(train,apply( (predmatrix-credit_amount)^2,2,mean))
head(test.error)
plot(n.trees, test.error, pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#Plot shows optimal value of n.trees to be 16000
pred<-predict(gbm_1,test,n.trees = 16000)
rmse <- rmse(pred,test$credit_amount)
print(rmse)


normalization_constant <- 100000
evaluation_metric <- ((1-rmse)/normalization_constant)*100
print(evaluation_metric)


#<-------Predictions-------->
set.seed(26)
gbm_2 <- gbm(credit_amount ~., data = train_master, cv.folds = 4,distribution = "gaussian", n.trees = 17000, shrinkage = 0.001, interaction.depth = 6)
pred_f <- predict(gbm_2, test_master, n.trees = 17000)
#Rounding off the values
credit_amount <- round(pred_f)
serial.number <- test_master$serial.number 

#<---------Output----------->
ans <- cbind(serial.number,credit_amount)
write.csv(ans, file = "submission_19.csv", row.names = FALSE)