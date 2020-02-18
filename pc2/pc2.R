library(rpart)
library(rpart.plot)

train <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc2/M2019btrain.csv")
summary(train)
boxplot(train$SCORE~train$GRADE)

# First model
model1 <- rpart(GRADE ~ SCORE+PARTICIPATION, data = train)
rpart.plot(model1)
CrossValidation::cross_validate(train, model1, 3, 0.8)

# Second model
model2 <- rpart(GRADE ~ SCORE+PARTICIPATION, control = rpart.control(minsplit = 100), data = train)
rpart.plot(model2)
CrossValidation::cross_validate(train, model2, 5, 0.8)

# Third model
model3 <- rpart(GRADE ~ SCORE+PARTICIPATION+LEAVES_EARLY+ASKS_QUESTIONS, control = rpart.control(minbucket = 30), data = train)
rpart.plot(model3)
CrossValidation::cross_validate(train, model3, 5, 0.8)

model4 <- rpart(GRADE~SCORE+PARTICIPATION, control = rpart.control(cp = 0.00001, minsplit = 6, minbucket = 7), data = train)
rpart.plot(model4)
CrossValidation::cross_validate(train, model4, 5, 0.08)

rPredict <- predict(model4, newdata = train, type  = "class")
train$PROJECTION <- rPredict
error <- mean(train$GRADE!=rPredict)
error

rPredict <- predict(model1, newdata = train, type  = "class")
train$PROJECTION <- rPredict
error <- mean(train$GRADE!=rPredict)
error


test <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc2/M2019_test_students.csv")
testPrediction <- predict(model1, newdata = test, type = "class")
test$X <- testPrediction

CrossValidation::cross_validate(train, model1, 5, 0.8)

M2019_sample_submission <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc2/M2019_sample_submission.csv")
M2019_sample_submission$GRADE <- testPrediction

write.csv(M2019_sample_submission, file = "prediction2.csv", row.names = FALSE)

