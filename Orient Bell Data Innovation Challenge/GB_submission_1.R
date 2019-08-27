#Orient Bell Qualifier Challenge
#Submitted by Team Prayaas
#Leaderboard rank at the time of submission : 2
#Latest score as per the leaderboard : 99.996412

#Language : R
#Best Model : Gradient Boosting
set.seed(26)

#install.packages("gbm")
library(gbm) #Gradient Boosting Package

#install.packages("Metrics")
library(Metrics) #RMSE calculation

#<---------Data Loading----------------->
#Master_Test & Train -> Original data
train_master <- read.csv("training_obl.csv")
test_master <- read.csv("test_obl.csv")
train_master <- subset(train_master, select=-c(id))

#Test  & Train split from orginal train data
cutoff = round(0.7*nrow(train_master)) #70:30 split for train and test respectively
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]

#Gradient Boosting Model
set.seed(26)

#After hyper parameter tuning
gbm_1 <- gbm(Price ~., data = train, cv.folds = 4,distribution = "gaussian", n.trees = 1000000, shrinkage = 0.0009, interaction.depth = 1)
plot(gbm_1, i="Nbedrooms")
plot(gbm_1, i="Nwashrooms")
n.trees = seq(from=100 ,to=400000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm_1,train,n.trees = n.trees)
dim(predmatrix)
test.error<-with(train,apply( (predmatrix-Price)^2,2,mean))
head(test.error)
plot(n.trees, test.error, pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#Plot shows optimal value of n.trees to be 1000000
best.iter <- gbm.perf(gbm_1, method = "OOB")
best.iter <- gbm.perf(gbm_1, method = "test")
best.iter <- gbm.perf(gbm_1, method = "cv")

# Function that returns Root Mean Squared Error
pred<-predict(gbm_1,test,n.trees = best.iter)
rmse <- rmse(round(pred),test$Price)
print(rmse)
#2.215 RMSE

pred_train <- predict(gbm_1,train, n.trees = best.iter)
rmse_train <- rmse(round(pred_train), train$Price)
print(rmse_train)
normalization_constant <- 100000
evaluation_metric <- ((1-rmse)/normalization_constant)*100
print(evaluation_metric)

#<-------Predictions-------->
set.seed(26)
gbm_2 <- gbm(Price ~., data = train_master, cv.folds = 4,distribution = "gaussian", n.trees = 1000000, shrinkage = 0.0009, interaction.depth = 1)
best.iter <- gbm.perf(gbm_2, method = "OOB")
best.iter <- gbm.perf(gbm_2, method = "test")
best.iter <- gbm.perf(gbm_2, method = "cv")

colnames(test_master)[colnames(test_master)=="Nbedroom"] <- "Nbedrooms"
colnames(test_master)[colnames(test_master)=="Nwashroom"] <- "Nwashrooms"
pred_f <- predict(gbm_2, subset(test_master, select=-c(id)), n.trees = best.iter)
#Rounding off the values
Price <- round(pred_f)
id <- test_master$id

#<---------Output----------->
ans <- cbind(id,Price)
write.csv(ans, file = "submission_6.csv", row.names = FALSE)

#<---------------------------------END OF CODE FILE---------------------------------->