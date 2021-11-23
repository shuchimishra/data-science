#KNN algorithm
?caret


data(iris)
str(iris)
iris

nrow(iris)
n.point = nrow(iris)
n.point

samplint.rate = 0.7

?sample

training = sample(n.point, samplint.rate*n.point, replace = FALSE)
class(training)
training

set.seed(1234)
training = sample(n.point, samplint.rate*n.point, replace = FALSE)
class(training)
training

?setdiff
summary(training)
str(training)

testing=setdiff(1:nrow(iris),training)
testing

iris_train = subset(iris[training,])
summary(iris_train)

iris_test = subset(iris[testing,])
summary(iris_test)

testing == training #to check if the value is common
any(testing == training) #to check if the value is common

table(iris_train$Species)
table(iris_test$Species)

prop.table(table(iris_train$Species)) #gives the proportion to see the distribution

spc_train = iris_train[,5]
spc_test = iris_test[,5]
#rm(spc_test)
iris_train = iris_train[,-5]
iris_test = iris_test[,-5]

#iris_train = cbind(iris_train,spc_train) # to add species field back

normalize = function(x){
  return((x-min(x))/(max(x)-min(x))) #normalizing data by calculating is min/max function
}

?lapply
#lapply converts to list, sapply converts to dataframe and vapply to vector

iris_train_n=sapply(iris_train,normalize) #normalize function is the same function we created earlier 
summary(iris_train_n)
iris_test_n=sapply(iris_test,normalize)
summary(iris_test_n)

install.packages("class")
library("class")

predict_class = knn(iris_train_n,iris_test_n,spc_train,k=3,prob= TRUE) #predict species of test data set

spc_test != predict_class
misclassification_error = mean(spc_test!=predict_class) #this is called misclassification error
mean(spc_test==predict_class)

class(spc_test)
class(predict_class)

plot(iris[1:4], col=iris$Species)

#looping to determine the best value of K
predict_spc = NULL
error.rate = NULL
k_val = NULL
for (i in 1:25)
{
  set.seed(2017)
  predict_spc = knn(iris_train_n,iris_test_n,spc_train,k=i)
  error.rate[i] = mean(spc_test!=predict_spc)
  k_val[i] = i
}
error.rate
#k = 1:10
error = data.frame(k_val,error.rate)
plot(error)
plot(k_val,error.rate,type = "b", col = "red")

library(gmodels)
CrossTable(x = spc_test, y = predict_class, prop.chisq = FALSE) # to see which species have misclassification
  