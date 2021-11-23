##### Chapter 11: Improving Model Performance -------------------

# load the credit dataset
setwd("~/Desktop/class4")
credit <- read.csv("credit.csv")
library(caret)

## Creating a simple tuned model ----
# automated parameter tuning of C5.0 decision tree 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")

# summary of tuning results
m

# apply the best C5.0 candidate model to make predictions
p <- predict(m, credit)
table(p, credit$default)

# obtain predicted classes
head(predict(m, credit, type = "raw"))

# obtain predicted probabilities
head(predict(m, credit, type = "prob"))

## Customizing the tuning process ----
# use trainControl() to alter resampling strategy
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# estimate performance of ipred bagged trees
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

## Boosting ----

## Using C5.0 Decision Tree (not shown in book)
library(C50)
m_c50_bst <- C5.0(default ~ ., data = credit, trials = 100)

# auto-tune a boosted C5.0 decision tree
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
               metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

## Using AdaBoost.M1
library(adabag)

# create a Adaboost.M1 model
set.seed(300)
m_adaboost <- boosting(default ~ ., data = credit)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

# create and evaluate an Adaboost.M1 model using 10-fold-CV
set.seed(300)
adaboost_cv <- boosting.cv(default ~ ., data = credit)
adaboost_cv$confusion

# calculate kappa
library(vcd)
Kappa(adaboost_cv$confusion)

## XGBoost
library(xgboost)
num<-sapply(credit[,c(1,5)], as.numeric)
predictors <- data.matrix( num) 
head(predictors)
levels(credit$checking_balance)

label <- as.numeric(credit[,'default'])-1 
head(label)
head(credit$default)

# Using subsample makes boosting act like the random forest except that the sampling 
# is done without replacement. The shrinkage parameter eta is helpful to prevent 
# overfitting by reducing the change in the weights (a smaller change in the weights 
# means the algorithm is less likely to overfit to the training set).

xgb <- xgboost(data = predictors, label = label, objective = "binary:logistic", 
               params = list( subsample =0.75, eta = 0.1), nrounds = 100)

# Note that xgboost does not support the formula syntax, so the predictors need to be 
# converted to a data.matrix and the response needs to be converted to 0/ 1 variables. 
# The objective argument tells xgboost what type of problem this is; based on this, 
# xgboost will choose a metric to optimize.

pred <- predict( xgb, newdata = predictors) 
xgb_df <- cbind( credit, pred_default = pred >0.5, prob_default = pred) 
library(ggplot2)
ggplot( data = xgb_df, aes( x = amount, y = checking_balance, 
      color = pred_default, shape = pred_default)) + geom_point( alpha =0.6, size = 2)

table(pred>0.5, credit$default)

## Random Forests ----
# random forest with default settings
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit, type = "prob")
rf

error_df<-data.frame(error_rate=rf$err.rate[,'OOB'], num_trees=1:rf$ntree)

library(ggplot2)
ggplot(error_df, aes(x=num_trees, y=error_rate)) + geom_line()

rf_pred<-predict(rf, credit[,-17])
table(rf_pred, credit$default)

### What is happening inside the Random Forest
rf_all<- randomForest(default ~ ., data = credit, importance=TRUE)
rf_all

varImpPlot(rf_all, type=1)
varImpPlot(rf_all, type=2)
varImpPlot(rf_all)

## By the decrease in accuracy of the model if the values of a variable 
## are randomly permuted (type = 1). Randomly permuting the values has 
## the effect of removing all predictive power for that variable. 
## The accuracy is computed from the out-of-bag data (so this measure 
## is effectively a cross-validated estimate). 
## By the mean decrease in the Gini impurity score for all of the nodes 
## that were split on a variable (type = 2). This measures how much 
## improvement to the purity of the nodes that variable contributes. 
## This measure is based on the training set, and therefore less reliable 
## than a measure calculated on out-of-bag data.


# auto-tune a random forest
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf


