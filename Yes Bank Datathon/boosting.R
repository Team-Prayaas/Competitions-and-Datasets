set.seed(26)
#<------------------X Gradient Boosting------------------->
library(xgboost)
library(caret)

xgbt_1 <- train(credit_amount ~., data = train, method = "xgbTree", trControl = trainControl("cv", number = 10))
pred <- xgbt_1 %>% predict(test)
rmse(pred, test$credit_amount)
#RMSE : 1676.072
mean(pred == test$credit_amount)

credit_amount <- round(pred)
serial.number <- test_master$serial.number 

#<---------Output----------->
ans <- cbind(serial.number,credit_amount)
write.csv(ans, file = "submission_11.csv", row.names = FALSE)
xgbt_2 <- train(f, data = dum_train, method = "xgbTree", trControl = trainControl("cv", number = 10))
pred <- xgbt_2 %>% predict(dum_test)
rmse(pred, test$credit_amount)
#RMSE : 1678.745
mean(pred == test$credit_amount)

#<--------------Gradient Boosting-------------------->
library(gbm)
gbm_1 <- gbm(credit_amount ~., data = train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
plot(gbm_1, i="duration_month")
plot(gbm_1, i="purpose")

n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 
#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm_1,train,n.trees = n.trees)
dim(predmatrix)
test.error<-with(train,apply( (predmatrix-credit_amount)^2,2,mean))
head(test.error)
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

pred<-predict(gbm_1,test,n.trees = 10000)
rmse(pred,test$credit_amount)
#RMSE 1666.380
#<-------Revo BT

rxImport(inData = "Yes_Bank_Train.csv", outFile = "train.xdf")
file.xdf <- file.path("train.xdf")
train.xdf <- file.xdf[1:cutoff]
test.xdf <- file.xdf[-(1:cutoff)]
n <- colnames(train)
n <- n[-1]
f2 <- as.formula(paste("credit_amount~", paste(n[!n %in% "credit_amount"], collapse = " + ")))

rxbt_1 <- rxBTrees(f2, data = train, minSplit = 2, nTree = 500, lossFunction = "gaussian", learningRate = 0.1) 
pred <- rxPredict(rxbt_1, test)
rmse(pred$credit_amount_Pred, test$credit_amount)
accuracy(pred$credit_amount_Pred, test$credit_amount)
#RMSE : 1711.870 

rxbt_2 <- rxBTrees(f2, data = train, minSplit = 2, nTree = 300, lossFunction = "gaussian", learningRate = 0.1) 
pred <- rxPredict(rxbt_2, test)
rmse(pred$credit_amount_Pred, test$credit_amount)
accuracy(pred$credit_amount_Pred, test$credit_amount)
#RMSE : 1683.027 

rxbt_3 <- rxBTrees(f2, data = train, minsplit = 2, nTree = 300, lossFunction = "gaussian") 
pred <- rxPredict(rxbt_3, test)
rmse(pred$credit_amount_Pred, test$credit_amount)
accuracy(pred$credit_amount_Pred, test$credit_amount)
#RMSE : 1691.873 

rxbt_4 <- rxBTrees(f, data = as.data.frame(dum_train), minSplit = 2, nTree = 200, lossFunction = "gaussian", learningRate = 0.1) 
pred <- rxPredict(rxbt_4, df_dum_test)
rmse(pred$credit_amount_Pred, df_dum_test$credit_amount)
accuracy(pred$credit_amount_Pred, test$credit_amount)
#RMSE : 1682.687 

rxbt_5 <- rxDForest(f, data = as.data.frame(dum_train),nTree = 200, lossFunction = "gaussian") 
pred <- rxPredict(rxbt_5, df_dum_test)
rmse(pred$credit_amount_Pred, df_dum_test$credit_amount)
accuracy(pred$credit_amount_Pred, df_dum_test$credit_amount)
#RMSE : 1748.687 

rxbt_6 <- rxDForest(f2, data = train_master, nTree = 500, lossFunction = "gaussian") 
pred <- rxPredict(rxbt_6, test_master)
rmse(pred$credit_amount_Pred, test$credit_amount)
accuracy(pred$credit_amount_Pred, test$credit_amount)
#RMSE : 1688.025
