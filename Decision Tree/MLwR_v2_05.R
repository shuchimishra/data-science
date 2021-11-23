data(iris)
set.seed(300)
idx = sample(150,105)
iris_train = iris[idx,]
iris_test = iris[-idx,]

prop.table(table(iris_train$Species))
prop.table(table(iris_test$Species))

set.seed(300)
install.packages("C50")
library(C50)


iris_model = C5.0(iris_train[-5],iris_train$Species)
summary(iris_model)
plot(iris_model)

set.seed(300)
iris_pred = predict(iris_model, iris_test[-5])

library(gmodels)
CrossTable(iris_test$Species, iris_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual default','predicted default'))

table(iris_pred,iris_test$Species)
error = mean(iris_pred!=iris_test$Species)
error
