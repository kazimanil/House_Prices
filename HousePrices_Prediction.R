# Data Input & Libraries ----
rm(list = ls()); gc()
load("HousePrices_AfterEDA.RData")
library(randomForest)
library(inTrees)
library(xgboost)
library(Matrix)

# Last Minute Call for Data Mungling ----
train <- cbind(train[, c("Id", "SalePrice", "logSP")],
               train[, -c("Id","SalePrice", "logSP")])
x_train <- train[, 4:52]
x_train <- x_train[, -c("HouseStyle", "Exterior1st", "Exterior2nd")] # different factor levels
y_train <- train$logSP
test  <- cbind(test[, c("Id", "MiscVal")],
               test[, -c("Id", "MiscVal")])
test[is.na(NewBuilding), NewBuilding := "Not New"]
test[is.na(TotalBsmtSF), TotalBsmtSF := 0]
# train[Neighborhood %in% c("IDOTRR", "Mitchel"), .N, .(MSZoning, Neighborhood)][order(Neighboorhood, -N)]
test[is.na(MSZoning) & Neighborhood == "IDOTRR", MSZoning := "RM"]
test[is.na(MSZoning) & Neighborhood == "Mitchel", MSZoning := "RL"]
test[is.na(SaleType), SaleType := "Oth"]

x_test <- test[, 3:51]
x_test <- x_test[, -c("HouseStyle", "Exterior1st", "Exterior2nd")] # different factor levels

# Random Forest %14.65 RMSE ----
rf1000 <- randomForest(x = x_train, y = y_train,
                       xtest = x_test,
                       ntree = 1000, importance = TRUE)
rf1000_pred <- cbind(test[, c("Id", "MiscVal")],
                 logSP_pred = rf1000$test$predicted)
rf1000_pred <- rf1000_pred[, .(Id, SalePrice = 10^logSP_pred + MiscVal)]
fwrite(rf1000_pred, file = "Submission_HousePrices_RandomForest.csv")

# Random Forest Pruned ----
rf1000_imp <- cbind(Variables = rownames(rf1000$importance), 
                    as.data.table(rf1000$importance))
chosen_vars <- rf1000_imp[IncNodePurity > 0.5]$Variables

x_train2 <- x_train[, c("Neighborhood", "OverallQual", "Foundation", "TotalBsmtSF", "CentralAir", "GrLivArea", "GarageType", "GarageFinish", "GarageArea",
                        "logLotArea", "RemodeledAge", "RemodeledAge2", "Bath")]
x_test2  <- x_test[, c("Neighborhood", "OverallQual", "Foundation", "TotalBsmtSF", "CentralAir", "GrLivArea", "GarageType", "GarageFinish", "GarageArea",
                       "logLotArea", "RemodeledAge", "RemodeledAge2", "Bath")]
rf_pruned <- randomForest(x = x_train2, y = y_train,
                          xtest = x_test2,
                          ntree = 1000, importance = TRUE)
rf_prunepred <- cbind(test[, c("Id", "MiscVal")],
                     logSP_pred = rf_pruned$test$predicted)
rf_prunepred <- rf_prunepred[, .(Id, SalePrice = 10^logSP_pred + MiscVal)]
fwrite(rf_prunepred, file = "Submission_HousePrices_RandomForest_Pruned.csv")

# XG Boost %18,36 RMSE ----
xg_model <- xgboost(data = data.matrix(x_train), label = y_train,
                    silent = 1, booster = "gblinear", nrounds = 25)
ypred <- predict(xg_model, data.matrix(x_test))
xgb <- cbind(test[, c("Id", "MiscVal")],
             logSP_pred = ypred)
xgb <- xgb[, .(Id, SalePrice = 10^logSP_pred + MiscVal)]
fwrite(xgb, file = "Submission_HousePrices_XGBoost.csv")
