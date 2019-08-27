library(Metrics)
library(car)
#Master_Test & Train
train_master <- read.csv("Yes_Bank_Train.csv")
test_master <- read.csv("Yes_Bank_Test_int.csv")

#Test  & Train from master
cutoff = round(0.7*nrow(train_master))
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]
  
#<------------------Linear Regression -1 ------------------->
linear_model_1 <- lm(credit_amount ~., data = train)
summary(linear_model_1) 
#Residual Standard Error : 1789 on 511 df
#R squared : 0.6189 ; Adjusted : 0.5827
#F: 17.26 on 48 & 511 DF ; p < 2.2e-16
resid <- residuals(linear_model_1)
plot(resid)
predictions <- predict(linear_model_1, test)
rmse(test$credit_amount,predictions) #1740.149
accuracy(test$credit_amount, predictions) #0


#<----------------Linear Regression -2 ------------------------>
linear_model_2 <- lm(credit_amount ~ account_info + duration_month + gurantors + property_type + liables + telephone, data = train)
summary(linear_model_2)
#Residual Standard Error : 2037 on 548 df
#R squared : 0.4698; Adjusted : 0.4591
#F: 44.14 on 11 & 548 DF, p < 2.2e-16
resid <- residuals(linear_model_2)
plot(resid)
predictions <- predict(linear_model_2)
rmse(test$credit_amount, predctions) #1740.149
accuracy(test$credit_amount, predictions) #0


#<-------------------------Error Analysis--------------------------->
corp <- lapply(train_master, as.integer)
cor(as.data.frame(corp))
vif(linear_model_1)
vif(linear_model_2)


#<------Rx Lin_Mod
rx_fl_1 <- rxFastLinear(formula = credit_amount~., data = train, type = "regression")
lm_pred <- rxPredict(rx_fl_1,test)
RMSE(lm_pred$Score, test$credit_amount, na.rm = FALSE)
rmse(lm_pred$Score, test$credit_amount)
#RMSE : 1795.725

rx_fl_2 <- rxFastLinear(formula = f, data = as.data.frame(dum_train), type = "regression")
lm_pred <- rxPredict(rx_fl_2,df_dum_test)
RMSE(lm_pred$Score, df_dum_test$credit_amount, na.rm = FALSE)
#RMSE : 1751.975

#<--------
linear_model_1 <- lm(f, data = as.data.frame(dum_train))
summary(linear_model_1) 
#Residual Standard Error : 1789 on 511 df
#R squared : 0.6189 ; Adjusted : 0.5827
#F: 17.26 on 48 & 511 DF ; p < 2.2e-16
resid <- residuals(linear_model_1)
plot(resid)
predictions <- predict(linear_model_1, df_dum_test)
rmse(df_dum_test$credit_amount,predictions) #1740.149
accuracy(df_dum_test$credit_amount, predictions) #0
#RMSE: 1737.662