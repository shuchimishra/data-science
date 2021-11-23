
rm(list=ls())
setwd("C:/Users/shuch/Desktop/UCLA/R/Machine Learning/CLASS_MATERIALS")
bc_data = read.csv("wisc_bc_data.csv")

bc_data = bc_data[,-1]

summary(bc_data)
str(bc_data)


total.row = nrow(bc_data)

set.seed(1222)
training = sample(total.row, 0.7*total.row, replace = FALSE)
testing=setdiff(1:nrow(bc_data),training)

bc_train = subset(bc_data[training,])
bc_test = subset(bc_data[testing,])

diag_train = bc_train[,1]
diag_test = bc_train[,1]


bc_train = bc_train[,-1]
bc_test = bc_test[,-1]

?scale

bc_train_s = scale(bc_train,center = TRUE, scale = TRUE)
bc_test_s = scale(bc_test,center = TRUE, scale = TRUE)

install.packages("class")
library("class")

predict_class = knn(bc_train_s,bc_test_s,diag_train,k=3,prob= TRUE) #predict species of test data set

str(predict_class)
str(bc_test)
bc_test!=predict_class
mean(bc_test!=predict_class)
mean(bc_test==predict_class)

class(bc_test)
class(predict_class)
