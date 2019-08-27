library(tidyverse)
library(GGally)

#Master_Test & Train
train_master <- read.csv("Yes_Bank_Train.csv")
test_master <- read.csv("Yes_Bank_Test_int.csv")

#Test  & Train from master
cutoff = round(0.7*nrow(train_master))
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]

#<----------Neural Network-------->
library(neuralnet)

maxs <- apply(train_master, 2, max) 
mins <- apply(train_master, 2, min)

scaled <- as.data.frame(scale(train_master, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]
n <- names(train)
f <- as.formula(paste("credit_amount~", paste(n[!n %in% "credit_amount"], collapse = " + ")))
nn_model_1 <- neuralnet(f, data = train, linear.output = TRUE, hidden = c(2,5))

#<-----model 2
library(nnet)
set.seed(26)
start <- proc.time()[3]
model.nn <- train(credit_amount ~., data = train, method = "nnet")
predictions <- predict(model.nn, test[setdiff(names(train), "credit_amount")])
accuracy <- sum(predictions == test$credit_amount)/length(test$credit_amount)
print(accuracy)
#RMSE: 4230.975

#<---------model 3
one_hot <- dummyVars(~., train, fullRank = FALSE)
train_hot <- predict(one_hot, train) %>% as.data.frame()
model.nn_2 <- train(credit_amount ~., data = train_hot, method = "nnet")
summary(model.nn_2)
#RMSE: 4268.679

#<----------Model 4
scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

train2 <- train
train2 <- train2 %>%
  mutate_all(scale01)

dum_train <- model.matrix(~ credit_amount + account_info + duration_month +  credit_history + purpose + savings_account + employment_st + poi + personal_status + gurantors + resident_since + property_type + age + installment_type + housing_type + credits_no + job_type + liables + telephone + foreigner, data = train)
dum_test <- model.matrix(~ credit_amount + account_info + duration_month +  credit_history + purpose + savings_account + employment_st + poi + personal_status + gurantors + resident_since + property_type + age + installment_type + housing_type + credits_no + job_type + liables + telephone + foreigner, data = test)
n <- colnames(dum_train)
n <- n[-1]
f <- as.formula(paste("credit_amount~", paste(n[!n %in% "credit_amount"], collapse = " + ")))

nn_model_2 <- neuralnet(f, data = dum_train, hidden = 50)
nn_model_3 <- neuralnet(f, data = dum_train, hidden = 50, act.fct = "tanh")


#<-----------RxNeural

rx_nn_1 <- rxNeuralNet(formula = credit_amount~., data = train, type = "regression", numHiddenNodes = 500, numIterations = 100)
pred <- rxPredict(rx_nn_1, data = test)