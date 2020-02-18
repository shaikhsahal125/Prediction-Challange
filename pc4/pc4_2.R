library(rpart)
library(rpart.plot)
library(randomForest)
library(MASS)
library(e1071)
library(nnet)

train2 <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc4/Earnings_Train2019.csv")
summary(train2)

rf_model3 <- randomForest::randomForest(Earnings ~., data = train2)
rf_predict3 <- predict(rf_model3, newdata = train2)
goodOne <- mean((rf_predict3 - train2$Earnings)^2)
rf_model3
goodOne


lm_model2 <- lm(Earnings~Number_Of_Professional_Connections+GPA+Number_Of_Credits+Number_Of_Parking_Tickets+Graduation_Year+Height, data = train2)
lm_model2 <- lm(Earnings ~ ., data = train2)
lm_predict <- predict(lm_model2, newdata = train2)
lm_mean <- mean((lm_predict - train2$Earnings)^2)
lm_mean

newAtr2 <- (8.956 * train2$Number_Of_Professional_Connections) + (-58.280 * train2$GPA)

rf_model6 <- randomForest::randomForest(Earnings ~ GPA+Number_Of_Professional_Connections+Number_Of_Credits+newAtr2+Major+Graduation_Year+Number_Of_Parking_Tickets, ntree=10, mtry=7, data = train2)
rf_predict6 <- predict(rf_model6, newdata = train2)
goodThree <- mean((rf_predict6 - train2$Earnings)^2)
rf_model6
goodThree

newAtr3 <- sqrt(train2$GPA + train2$Number_Of_Professional_Connections + train2$Graduation_Year + train2$Number_Of_Credits)

rf_model7 <- randomForest::randomForest(Earnings ~ GPA+Number_Of_Professional_Connections+Number_Of_Credits+newAtr3+Major+Graduation_Year+Number_Of_Parking_Tickets, ntree=10, mtry=7, data = train2)
rf_predict7 <- predict(rf_model7, newdata = train2)
goodThree7 <- mean((rf_predict7 - train2$Earnings)^2)
rf_model7
goodThree7

new_atr <- ((train2$GPA - train2$Height)^2 / (train2$Number_Of_Professional_Connections - train2$Graduation_Year - train2$Number_Of_Credits - train2$Number_Of_Parking_Tickets))/8

new_rf_model <- randomForest::randomForest(Earnings~GPA+Number_Of_Professional_Connections+Number_Of_Credits+new_atr+Major+Graduation_Year+Number_Of_Parking_Tickets,ntree = 10, mtry = 7, data = train2)
new_rf_predict <- predict(new_rf_model, newdata = train2)
new_rf_mean <- mean((new_rf_predict - train2$Earnings)^2)
new_rf_mean

r_model2 <- rpart(Earnings ~., control = rpart.control(cp=0.000000001),data = train2)
r_prediction2 <- predict(r_model2, newdata = train2)
r_mean2 <- mean((r_prediction2 - train2$Earnings)^2)
r_mean2


nnet_model2 <- nnet(Earnings/24879 ~., data = train2, size = 70)
nnet_Predict2 <- predict(nnet_model2, newdata = train2) * 24879
nnet_mean <- mean((nnet_Predict2 - train2$Earnings)^2)
nnet_mean


test2 <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc4/Earnings_Test_Students.csv")
new_atr <- ((test2$GPA - test2$Height)^2 / (test2$Number_Of_Professional_Connections - test2$Graduation_Year - test2$Number_Of_Credits - test2$Number_Of_Parking_Tickets))/8
rf_predict_test <- predict(new_rf_model, newdata = test2)
test2$proj <- rf_predict_test

earning_submission <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc4/earning_submission.csv")
earning_submission$Earnings <- rf_predict_test

write.csv(earning_submission, file = "pc4_2.csv", row.names = FALSE)
