set.seed(26)
library(xgboost)
library(caret)
library(astsa)
require(data.table)
require(Matrix)
#setwd("D:/DataAnalytics_Files/Unilever")

train_master <- read.csv("training.csv")
test_master <- read.csv("test.csv")
head(train_master)

train_reg <- subset(train_master, select = -c(serial_number, time_stamp))
cutoff = round(0.7*nrow(train_reg)) #70:30 split for train and test respectively
train <- train_reg[1:cutoff,]
test <- train_reg[-(1:cutoff),]

sparse_matrix <- sparse.model.matrix(X5.year~., data = train)[,-1]
output_vector <- train[,"X5.year"]
dtrain <- xgb.DMatrix(data=as.matrix(train),label=output_vector, missing=NA)
dtest <- xgb.DMatrix(data=as.matrix(test))

#xgboost(data = dtrain, max_depth = 4, eta = 0.1, nthread =5, nrounds = 10000, objectivee = "reg:linear")


param <- list(booster = "gblinear"
              , objective = "reg:linear"
              , subsample = 0.5
              , max_depth = 9
              , colsample_bytree = 0.7
              , eta = 0.005
              , eval_metric = 'mae'
              , base_score = 0.012 #average
              , min_child_weight = 100)


# Perform xgboost cross-validation
# Won't fit under kernel limit. Uncomment to run locally. 
foldsCV <- createFolds(output_vector, k=7, list=TRUE, returnTrain=FALSE)
xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                 nrounds=50000,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 30,
                 print_every_n = 5
)

# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration

xgb <- xgb.train(params = param
                 , data = dtrain
                 # , watchlist = list(train = dtrain)
                 , nrounds = nrounds
                 , verbose = 1
                 , print_every_n = 5
                 #, feval = amm_mae
)
importance_matrix <- xgb.importance(names(dtrain),model=xgb)
preds <- predict(xgb,dtest)
confusionMatrix(round(preds,3),test$X5.year)



#Final
sparse_matrix <- sparse.model.matrix(X5.year~., data = train_reg)[,-1]
output_vector <- train_reg[,"X5.year"]
dtrain <- xgb.DMatrix(data=as.matrix(train_reg),label=output_vector, missing=NA)
test_master = subset(test_master, select = -c(time_stamp))
dtest <- xgb.DMatrix(data=as.matrix(test_master))

#xgboost(data = dtrain, max_depth = 4, eta = 0.1, nthread =5, nrounds = 10000, objectivee = "reg:linear")


param <- list(booster = "gblinear"
              , objective = "reg:linear"
              , subsample = 0.5
              , max_depth = 9
              , colsample_bytree = 0.7
              , eta = 0.005
              , eval_metric = 'mae'
              , base_score = 0.012 #average
              , min_child_weight = 100)


# Perform xgboost cross-validation
# Won't fit under kernel limit. Uncomment to run locally. 
foldsCV <- createFolds(output_vector, k=7, list=TRUE, returnTrain=FALSE)
xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                 nrounds=50000,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 30,
                 print_every_n = 5
)

# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration

xgb <- xgb.train(params = param
                 , data = dtrain
                 # , watchlist = list(train = dtrain)
                 , nrounds = nrounds
                 , verbose = 1
                 , print_every_n = 5
                 #, feval = amm_mae
)
importance_matrix <- xgb.importance(names(dtrain),model=xgb)

preds <- predict(xgb,as.matrix(test_master))
preds <- round(preds,3) 
df_result <- data.frame(test_master$serial_number,preds)
write.csv(df_result,"submission2.csv")