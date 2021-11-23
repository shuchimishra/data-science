install.packages("ipred")
library(ipred)


credit = read.csv("credit.csv")
credit

#create ensemble using bagging technique
set.seed(300)
mybag = bagging(default ~ ., data = credit, nbagg = 25)

#predict the data
credit_pred = predict(mybag, credit)

table(credit_pred, credit$default)

#using train control parameters to further optimize model's performance 
library(caret)
set.seed(300)
ctrl = trainControl(method = "cv", number = 10)
set.seed(300)
train(default ~ .,data = credit, method = "treebag", trControl = ctrl)
