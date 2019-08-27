library(Metrics)
library(rpart)
#Master_Test & Train
train_master <- read.csv("Yes_Bank_Train.csv")
test_master <- read.csv("Yes_Bank_Test_int.csv")

#Test  & Train from master
cutoff = round(0.7*nrow(train_master))
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]

#<----------------Decision Tree------------------------>
tree_model_1 <- rpart(credit_amount ~. - serial.number, data = train_master, method = "anova")
summary(tree_model_1)
printcp(tree_model_1)
plotcp(tree_model_1)
rsq.rpart(tree_model_1)
print(tree_model_1)
plot(tree_model_1)
text(tree_model_1)
post(tree_model_1, file = "tree_model_1.ps")

#Predict
predictions <- predict(tree_model_1, test_master)
accuracy(test$credit_amount, predictions) #0
rmse(test$credit_amount, predictions) #1956.012
#RMSE : 1956.012
#Pruning Tree
pruned_tree <- prune(tree_model_1, cp = tree_model_1$cptable[which.min(tree_model_1$cptable[,"xerror"]),"CP"])
printcp(pruned_tree)
rsq.rpart(pruned_tree)
plot(pruned_tree)

#Pruned_Predict
predictions <- predict(pruned_tree, test)
accuracy(test$credit_amount, predictions)
rmse(test$credit_amount, predictions) #1978.902



#<------------Revo Trees
rx_ft_1 <- rxFastTrees(credit_amount ~., data = train_master, type = "regression")
pred_ft <- rxPredict(rx_ft_1, data = test_master)
rmse(test$credit_amount, pred_ft$Score)
#RMSE : 1802.496

rx_ft_2 <- rxFastTrees(formula = f, data = as.data.frame(dum_train), type = "regression")
pred_ft <- rxPredict(rx_ft_2, data = df_dum_test)
rmse(df_dum_test$credit_amount, pred_ft$Score)
#RMSE : 1804.263

