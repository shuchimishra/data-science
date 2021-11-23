data(iris)
set.seed(300)
idx<-sample(150,105)
iris_train<-iris[idx,]
iris_test<-iris[-idx,]

prop.table(table(iris$Species))
prop.table(table(iris_train$Species))
prop.table(table(iris_test$Species))

install.packages("caret")
library(caret)
set.seed(300)
idx1<-createDataPartition(iris$Species, p=0.7, list=FALSE)
iris_train_i<-iris[idx1,]
iris_test_i<-iris[-idx1,]

prop.table(table(iris$Species))
prop.table(table(iris_train_i$Species))
prop.table(table(iris_test_i$Species))

set.seed(300)
install.packages("C50")
library(C50)

iris_model <- C5.0(iris_train[-5], iris_train$Species)
summary(iris_model)
plot(iris_model)

set.seed(300)
iris_pred <- predict(iris_model, iris_test[-5])

library(gmodels)
CrossTable(iris_test$Species, iris_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

table(iris_pred, iris_test$Species)
error<-mean(iris_pred!=iris_test$Species)
error

set.seed(300)
iris_model_i <- C5.0(iris_train_i[-5], iris_train_i$Species)
summary(iris_model_i)
plot(iris_model_i)

set.seed(300)
iris_pred_i <- predict(iris_model_i, iris_test_i[-5])

library(gmodels)
CrossTable(iris_test_i$Species, iris_pred_i,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

table(iris_pred_i, iris_test_i$Species)
error_i<-mean(iris_pred_i!=iris_test_i$Species)
error_i

##Using Caret###
set.seed(300)
m_cv <- train(Species ~ ., data = iris, method = "C5.0")
m
set.seed(300)
p <- predict(m, iris)
table(p, iris$Species)

error_c<-mean(p!=iris$Species)
error_c

ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "best") ### or selectionFunction =  "oneSE"

set.seed(300)
m1 <- train(Species ~ ., data = iris, method = "C5.0", trControl = ctrl)

set.seed(300)
p <- predict(m1, iris)
table(p, iris$Species)
error_c2<-mean(p!=iris$Species)
error_c2