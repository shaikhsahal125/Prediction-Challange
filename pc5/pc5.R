library(rpart)
library(rpart.plot)
library(e1071)

train <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc5/P5_train1.csv")
summary(train)

rmodel <- rpart(Revenue ~ Duration+Activity+Source, data = train)
rpart.plot(rmodel)
rpredict <- predict(rmodel, newdata = train)
rmse <- mean((rpredict - train$Revenue)^2)
rmse

# Random Forest
rf_model <- randomForest::randomForest(Revenue ~ Duration+Activity+Source, ntree = 100, mtry = 3, data = train)
rf_predict <- predict(rf_model, newdata = train)
rf_mse <- mean((rf_predict - train$Revenue)^2)
rf_mse

# SVM model
svm_model <- svm(Revenue ~ Duration+Activity+Source, cost = 100, gamma = 100000, epsilon = 0.01, data = train)
svm_predict <- predict(svm_model, newdata = train)
svm_mse <- mean((svm_predict - train$Revenue)^2)
train$PRED <- svm_predict
svm_mse


test1 <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc5/P5_test1_students.csv")
test_predict <- predict(svm_model, newdata = test1)
test1$PredRevenue <- test_predict

revenue_submission <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc5/revenue_submission.csv")
revenue_submission$Revenue <- test_predict

write.csv(revenue_submission, file = "pc5.csv", row.names = FALSE)
