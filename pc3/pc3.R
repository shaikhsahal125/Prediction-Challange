library(rpart)
library(rpart.plot)

# Training data
train <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc3/Tillet-Analytics_train.csv")
summary(train)



model1 <- rpart(Party ~ Social.Media+Favorite.song+Calories+Coffees, data = train)
rpart.plot(model1)
CrossValidation::cross_validate(train, model1, 3, 0.8)

model2 <- rpart(Party ~ Favorite.song+Calories+Coffees, control = rpart.control(cp = 0.0001, minsplit = 6, minbucket = 3), data = train)
rpart.plot(model2)
CrossValidation::cross_validate(train, model2, 10, 0.8)

model3 <- rpart(Party~., control = rpart.control(minsplit = 125, minbucket = 63), data = train)
rpart.plot(model3)
CrossValidation::cross_validate(train, model3, 10, 0.8)


rPredict <- predict(model3, newdata = train, type = "class")
error <- mean(train$Party!=rPredict)
error

# Testing Data 
test <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc3/Tillet_Analytics_students.csv")
testPredict <- predict(model2, newdata = test, type = "class")
test$Party <- testPredict

# Submisson
Tillet_Submisson <- read.csv("~/Rutgers/Spring_2019/DATA-101/Assignments/pc3/Tillet_Submisson.csv")
Tillet_Submisson$Party <- testPredict

write.csv(Tillet_Submisson, file = "pc3.csv", row.names = FALSE)
