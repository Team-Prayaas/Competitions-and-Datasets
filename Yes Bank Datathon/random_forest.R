#Master_Test & Train
train_master <- read.csv("Yes_Bank_Train.csv")
test_master <- read.csv("Yes_Bank_Test_int.csv")

#Test  & Train from master
cutoff = round(0.7*nrow(train_master))
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]


#<------------Random FOrest----------->
set.seed(26)
library(randomForest)

#<-------Model 1
forest_model_1 <- randomForest(formula = credit_amount ~., data = train_master, ntree = 17000, mtry = 1)
plot(forest_model_1)
which.min(forest_model_1$mse)
sqrt(forest_model_1$mse[which.min(forest_model_1$mse)])
#RMSE:1934.609

pre <- predict(forest_model_1,test_master)
pre <- round(pre)
credit_amount <- round(pred_f)
serial.number <- test_master$serial.number 

#<---------Output----------->
ans <- cbind(serial.number,credit_amount)
write.csv(ans, file = "submission_11.csv", row.names = FALSE)



#<-------Model 2
forest_model_2 <- randomForest(formula = credit_amount ~., data = train, xtest = subset(test, select = -c(credit_amount)), ytest = test$credit_amount)
oob <- sqrt(forest_model_2$mse)
validation <- sqrt(forest_model_2$test$mse)

tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:forest_model_2$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

which.min(forest_model_2$mse)
sqrt(forest_model_2$mse[which.min(forest_model_2$mse)])
#RMSE: 1950.895


#<-------Model 3
forest_model_3 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 2)
plot(forest_model_3)
which.min(forest_model_3$mse)
sqrt(forest_model_3$mse[which.min(forest_model_3$mse)])
#RMSE:2077.723

#<-------Model 4
forest_model_4 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 3)
plot(forest_model_4)
which.min(forest_model_4$mse)
sqrt(forest_model_4$mse[which.min(forest_model_4$mse)])
#RMSE:2010.773

#<-------Model 5
forest_model_5 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 4)
plot(forest_model_5)
which.min(forest_model_5$mse)
sqrt(forest_model_5$mse[which.min(forest_model_5$mse)])
#RMSE:1987.813

#<-------Model 6
forest_model_5 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 5)
plot(forest_model_5)
which.min(forest_model_5$mse)
sqrt(forest_model_5$mse[which.min(forest_model_5$mse)])
#RMSE:1966.804

#<-------Model 7
forest_model_7 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 10)
plot(forest_model_7)
which.min(forest_model_7$mse)
sqrt(forest_model_7$mse[which.min(forest_model_7$mse)])
#RMSE:1937.181

#<-------Model 8
forest_model_8 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 12)
plot(forest_model_8)
which.min(forest_model_8$mse)
sqrt(forest_model_8$mse[which.min(forest_model_8$mse)])
#RMSE:1934.564

#<-------Model 9
forest_model_9 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 13)
plot(forest_model_9)
which.min(forest_model_9$mse)
sqrt(forest_model_9$mse[which.min(forest_model_9$mse)])
#RMSE:1960.142


#<------Model 10
features <- setdiff(names(train), "credit_amount")
tune_forest_model_10 <- tuneRF(x = train[features], y = train$credit_amount, ntreeTry = 500, mtryStart = 5, stepFactor = 1.5, improve = 0.01, trace = TRUE)
forest_model_10 <- randomForest(formula = credit_amount ~., data = train, ntree = 1000, mtry = 13)
which.min(forest_model_10$mse)
sqrt(forest_model_10$mse[which.min(forest_model_10$mse)])

#<--------Model 11
one_hot <- dummyVars(~., train, fullRank = FALSE)
train_hot <- predict(one_hot, train) %>% as.data.frame()

names(train_hot) <- make.names(names(train_hot), allow_ = FALSE)
hyper_grid_2 <- expand.grid(
  mtry       = seq(1, 21, by = 1),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE  = 0
)

for(i in 1:nrow(hyper_grid_2)) {
  
  # train model
  model <- ranger(
    formula         = credit.amount ~ ., 
    data            = train_hot, 
    num.trees       = 500,
    mtry            = hyper_grid_2$mtry[i],
    min.node.size   = hyper_grid_2$node_size[i],
    sample.fraction = hyper_grid_2$sampe_size[i],
    seed            = 26
  )
  
  # add OOB error to grid
  hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

summary(model)
#R squared : 0.516




#<------------H20

library(h2o)

h2o.no_progress()
h2o.init(max_mem_size = "8g")
y <- "credit_amount"
x <- setdiff(names(train), y)

train.h2o <- as.h2o(train)
# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(1, 21, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)


#<-------------RevoScale Forests
rxff_1 <- rxFastForest(formula = credit_amount ~., data = train, type = "regression", numTrees = 16000, numLeaves = 100, gainConfLevel = 0.5)
rxFastForestScore <- rxPredict(rxff_1, data = test, 
                                 writeModelVars = TRUE)
rxLinePlot(Score ~ credit_amount, type = c("p", "smooth"), data = rxFastForestScore)
rmse(test$credit_amount, rxFastForestScore$Score)
#RMSE : 179..
rxff_1 <- rxFastForest(formula = credit_amount ~., data = train_master, type = "regression", numTrees = 16000, numLeaves = 100, gainConfLevel = 0.5)
rxFastForestScore <- rxPredict(rxff_1, data = test_master )









########

rxff_2 <- rxFastForest(formula = f, data = as.data.frame(dum_train), type = "regression")
rxFastForestScore <- rxPredict(rxff_2, data = as.data.frame(dum_test), 
                               writeModelVars = TRUE)
rmse(df_dum_test$credit_amount, rxFastForestScore$Score)
#RMSE : 1823.904