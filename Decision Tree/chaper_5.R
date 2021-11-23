#Identifying Risky Bank loans
#Step2 : Exploring and preparing the data

credit = read.csv("credit.csv")
str(credit)

#Look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

#look at tow characteristics of loan
summary(credit$months_loan_duration)
summary(credit$amount)

#look at the class variable
table(credit$default)

#create a random sample for testing and test data
#use sed.seed to use random number sequence
set.seed(300)
train_sample = sample(1000,900)
str(train_sample)

#split the data frames
credit_train = credit[train_sample,]
credit_test = credit[-train_sample,]

library(caret)
set.seed(300)
index = createDataPartition(credit$default, p=0.9, list = FALSE) #stratified sampling
?createDataPartition
credit_train_i = credit[index,]
credit_test_i = credit[-index,]

#check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

prop.table(table(credit_train_i$default))
prop.table(table(credit_test_i$default))

##Step#3 Training a model based on train data
#build the simplest decision tree
library(C50)
set.seed(300)

?C5.0
credit.model = C5.0(credit_train[-17], credit_train$default)
credit.model_i = C5.0(credit_train_i[-17], credit_train_i$default)

summary(credit.model_i)
summary(credit.model)
#plot(credit_model)

##step4: Evaluating model performance
#create a factor vector on prediction of test data
credit = predict(credit.model, credit_test)
credit_i = predict(credit.model_i, credit_test_i)

#cross tabulation of predicted versus actual classes
library(gmodels)

CrossTable(credit_i, credit_test_i$default, prop.chisq = FALSE)
CrossTable(credit, credit_test$default, prop.chisq = FALSE)

#Boosted decision tree with 10 trials
set.seed(300)
credt_boot10 = C5.0(credit_train_i[-17], credit_train_i$default, trials = 10)

credit_boost10
summary(credit_boost10)


credit_boost10$boostResults
credit_boost_pred10 = predict(credit_boost10, credit_test_i)
head(predict(credit_boost10, credit_test_i, type = "prob"))

CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, 
           prop.r = FALSE, prop.c = FALSE,
           dnn = c('actual default', 'predicted default'))

##Making some matrix is costly than others

#creating dimensions of cost matrix
matrix_dimensions = list(c("no","yes"), c("no", "yes"))
names(matrix_dimensions) = c("predicted", "actual")

#build the matrix
error_cost = matrix(c(0,1,4,0), nrow = 2, dimnames = matrix_dimensions)

#apply the cost matrix to tree
credit_cost = C5.0(credit_train_i[-17], credit_train_i$default, costs = error_cost)
credit_cost_pred = predict(credit_cost, credit_test_i)

CrossTable(credit_test_i$default, credit_cost_pred, prop.chisq = FALSE)

##Bagging
#using the ipred bagged decision trees
library(ipred)
set.seed(300)

credit = read.csv("credit.csv")
mybag = bagging(default ~., data = credit, nbagg = 25)
credit_pred = predict(mybag,credit)
table(credit_pred, credit$default)

#estimating the performance of ipred bagged trees
library(caret)
set.seed(300)
crtl = trainControl(method = "cv", number = 10)
train(default ~.,data = credit, method = "treebag", trControl = ctrl)

##using C5.0 decision tree (not shown in book)