

install.packages("mlbench")
library(mlbench)


data("HouseVotes84")
str(HouseVotes84)

summary(HouseVotes84)

total = nrow(HouseVotes84)
set.seed(1212)
training = sample(total, 0.7*total, replace = FALSE)
testing=setdiff(1:total,training)

house_train = subset(HouseVotes84[training,])
str(house_train)

house_test = subset(HouseVotes84[testing,])
str(house_test)

install.packages("e1071")
library(e1071)

?naiveBayes
model = naiveBayes(Class~.,data = house_train) #if we want to pull for just V1,V2 and V3 - naiveBayes(Class~V1+V2+V3,data = house_train)
summary(model)
model

class_predict = predict(model, house_test)
?predict

str(class_predict)

library(gmodels)
CrossTable(x=house_test$Class, y=class_predict, prop.chisq = TRUE)
