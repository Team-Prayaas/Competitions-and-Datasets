#Unilever Datathon Qualifier Challenge
#Team Prayaas
#Rank at the time of submission : 10th
#Score at the time of submission : 98.245842
#Language : R
#Final Model : Gradient Boosting

setwd("D:/DataAnalytics_Files/Unilever Datathon")
set.seed(26)
library(gbm) #Gradient Boosting Package
library(Metrics) #RMSE calculation

#<---------Data Loading----------------->
#Original Train & Test File
train_master <- read.csv("UNI.csv")
test_master <- read.csv("unitest.csv")

#Test  & Train split from original file
cutoff = round(0.7*nrow(train_master)) #70:30 split for train and test respectively
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]

#Dropping v.id from model
train <- subset(train, select = -c(v.id))
test <- subset(test, select = -c(v.id))

#Gradient Boosting Model
set.seed(26)
gbm_1 <-  gbm(current.price ~., data = train, cv.folds = 5,distribution = "gaussian", n.trees = 5000000, shrinkage = 0.005, interaction.depth = 1)
n.trees = seq(from=100 ,to=5000000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm_1,train,n.trees = n.trees)
dim(predmatrix)
test.error<-with(train,apply( (predmatrix-current.price)^2,2,mean))
head(test.error)
plot(n.trees, test.error, pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#Plot shows optimal value of n.trees to be 5000000
best.iter <- gbm.perf(gbm_1, method = "OOB")
best.iter <- gbm.perf(gbm_1, method = "test")
best.iter <- gbm.perf(gbm_1, method = "cv")

#Predictions on train set
pred<-predict(gbm_1,test,n.trees = best.iter)
rmse <- rmse(round(pred, digits = 1),test$current.price)
print(rmse)
normalization_constant <- 100000
evaluation_metric <- ((1-rmse)/normalization_constant)*100
print(evaluation_metric)

#<-------Predictions-------->
set.seed(26)
gbm_2 <- gbm(current.price ~., data = train_master, cv.folds = 5,distribution = "gaussian", n.trees = 1000000, shrinkage = 0.005, interaction.depth = 1)
best.iter <- gbm.perf(gbm_2, method = "OOB")
best.iter <- gbm.perf(gbm_2, method = "test")
best.iter <- gbm.perf(gbm_2, method = "cv")

#Predictions on original test set
pred_f <- predict(gbm_2, test_master, n.trees = best.iter)

#Rounding off the values
current.price <- round(pred_f, digits = 1)
v.id <- test_master$id

#<---------Output----------->
ans <- cbind(v.id,round(current.price*2)/2) #Since the train file contains rounded values or decimals with 0.5
write.csv(ans, file = "submission_5.csv")

#<---------------------------------END OF CODE FILE---------------------------------->