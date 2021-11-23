##### Chapter 11: Improving Model Performance -------------------

# load the credit dataset
credit <- read.csv("credit.csv")
str(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$amount)
summary(credit$months_loan_duration)
table(credit$default)

library(caret)
set.seed(310)
train_sample = sample(1000,900)

credit_train = credit[train_sample,]
credit_test = credit[-train_sample,]


prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

install.packages("C50")
library(C50)
?C5.0Control


set.seed(310)
credit_model = C5.0(credit_train[-17],credit_train$default)
summary(credit_model)

set.seed(310)
credit_pred = predict(credit_model, credit_test)
prop.table(table(credit_pred))

library(gmodels)
set.seed(310)
CrossTable(credit_test$default,credit_pred, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, dnn= c("actual default","predicted value"))

mean(credit_test$default != credit_pred)

#Boosting model performance
set.seed(310)
credit_boost10 = C5.0(credit_train[-17], credit_train$default, trials = 10)
summary(credit_boost10)


set.seed(310)
credit_boost_pred10 = predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.c = FALSE, prop.r = FALSE, prop.chisq = FALSE, dnn = c("actual default","predicted value"))
mean(credit_test$default != credit_boost_pred10)

#Building cost matrix
matrix_dimension = list(c("No", "Yes"), c("No", "Yes"))
names(matrix_dimension) = c("Predicted", "Actual")

error_cost = matrix(c(0,1,0,4), nrow = 2, dimnames = matrix_dimension)
set.seed(310)
credit_cost = C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred =  predict(credit_cost, credit_test)

set.seed(310)
CrossTable(credit_test$default, credit_cost_pred, prop.r = FALSE, prop.c = FALSE, prop.chisq = FALSE,  dnn = c("actual default","predicted value"))  
  
install.packages("caret")
library(caret)
modelLookup("C5.0")
## Creating a simple tuned model ----
# automated parameter tuning of C5.0 decision tree 
set.seed(310)
m = train(default ~., data = credit, method = "C5.0")

# summary of tuning results
m
str(m)
m$finalModel
# apply the best C5.0 candidate model to make predictions
p = predict(m,credit)
prop.table(table(p,credit$default))

?trainControl
?best
# obtain predicted classes
head(predict(m, credit, type = "raw"))

# obtain predicted probabilities
head(predict(m, credit, type = "prob"))

## Customizing the tuning process ----
# use trainControl() to alter resampling strategy
ctrl = trainControl(method = "cv", number = 10, selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters
grid = expand.grid(.model = "tree", 
                   .trials = c(1,5,10,15,20,25,30,35),
                   .winnow = "FALSE")

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
set.seed(310)
m = train(default ~., data = credit, method = "C5.0",
          metric = "Kappa", trControl = ctrl,
          tuneGrid = grid)
m

p = predict(m,credit)
prop.table(table(p,credit$default))
## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
set.seed(310)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# estimate performance of ipred bagged trees
library(caret)
set.seed(310)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

# Using caret's more general bagging function
# create a bag control object using svmBag
str(svmBag)
svmBag$fit

bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)

# fit the bagged svm model
set.seed(310)
svmbag <- train(default ~ ., data = credit, "bag",
                trControl = ctrl, bagControl = bagctrl)

svmbag

## Boosting ----

## Using C5.0 Decision Tree (not shown in book)
library(C50)
m_c50_bst <- C5.0(default ~ ., data = credit, trials = 100)

## Using AdaBoost.M1
library(adabag)

# create a Adaboost.M1 model
set.seed(310)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

# create and evaluate an Adaboost.M1 model using 10-fold-CV
set.seed(310)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

# calculate kappa
library(vcd)
Kappa(adaboost_cv$confusion)

## Random Forests ----
# random forest with default settings
library(randomForest)
set.seed(310)
rf <- randomForest(default ~ ., data = credit)
rf

library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(310)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf

# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

set.seed(310)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
                metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50
