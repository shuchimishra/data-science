### KNN using caret package on iris dataset ###
install.packages("caret")
library(caret)

# Splitting the data
n.point<-nrow(iris)
sampling.rate<-0.7
# Identifying rows for training dataset
set.seed(101)
training<-sample(1:n.point, sampling.rate*n.point, replace=FALSE)

# Setting the difference for testing dataset
testing<-setdiff(1:n.point, training)

# Defining training and testing iris data
iris_train<-subset(iris[training,])
iris_test<-subset(iris[testing,])

# Saving the labels 
spc_test<-iris_test[,5]
spc_train<-iris_train[,5]

# KNN using caret function train()
# Note: We are not removing the Species column from our train dataset
set.seed(101)
model<-train(Species~., data=iris_train, method="knn")

# Review model details
model

# Predict using model
set.seed(101)
predict_spc<-predict(model, iris_test)

# Review model performance
library(gmodels)
CrossTable(x=spc_test, y=predict_spc, prop.chisq = FALSE)

# Adding pre-processing steps: scaling and centering data
set.seed(101)
model2<-train(Species~., data=iris_train, method="knn", preProcess=c("center", "scale"))
model2

# Predict using model2
set.seed(101)
predict_spc2<-predict(model2, iris_test)

# Review model performance
library(gmodels)
CrossTable(x=spc_test, y=predict_spc2, prop.chisq = FALSE)

