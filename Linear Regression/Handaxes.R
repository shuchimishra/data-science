# Catalog Specimen catalog number. 
# L Maximum Length. 
# L1 Distance butt. 
# B Maximum breadth. 
# B1 Breadth from the tip. 
# B2 Breadth from the butt. 
# T Maximum thickness. 
# T1 Thickness measured at B1.

# Reviewing data

data("Handaxes", package = "archdata")
summary(Handaxes[,3:5])
any(is.na(Handaxes))
library(psych)
pairs.panels(Handaxes)


# The relationship between L and each attribute is positive, as indicated by the upward slope in the scatter plots; and the positive correlation coefficients. 
# It appears L has a high positive correlation with the features with B (0.77), B2(0.70) and T(0.61).
# The attributes are all positively correlated with each other. For example, as the maximum breadth increases, so do all the other attributes. This is to be expected - larger handaxes have larger dimensions all round.
# Several of the attributes are highly correlated, for example, B2 has a 0.85 correlation with B; and B1 has a 0.75 correlation with L1.
# The density plots, appear to indicate the attributes are approximately bell shaped. Thus, they might be generated from the normal distribution.

# Preparing the data

# In regression modeling, a standard approach is to apply a natural log transformation to both the target variable and the attributes before fitting the model. 
# When a simple linear regression model is fitted to logged variables, the slope coefficient represents the predicted percent change in the target variable per percent change in the attribute variable. 

y <- log (data.matrix (Handaxes [,2])) 
data_sample <- log (data.frame (data.matrix ( Handaxes [,3:8])))


# The findCorrelation takes two primary arguments. The first is a correlation matrix, and the second is a correlation cutoff. We use a cutoffof 0.6: 
cor_matrix <- cor (data.matrix (data_sample)) 
require (caret) 
rid_col <- findCorrelation(cor_matrix, cutoff = 0.6, exact = FALSE)
rid_col
colnames(data_sample)[rid_col]

data_sample$B1<-NULL
data_sample$B<-NULL

# Test and Train sets
set.seed(2016)
N=nrow(data_sample)
train=sample(1:N, 500, FALSE)
y_train<-y[train]
y_test<-y[-train]
data_train<-data_sample[train,]
data_test<-data_sample[-train,]

# Train Model 
fit<-lm(y_train ~ ., data = data_train)
summary(fit)

confint(fit, level = 0.95)

par(mfrow=c(2,2))
plot(fit)

plot(fit, which = 1)

# Density of Residuals. We are looking for a bell shaped curve

plot(density(fit$residuals))

# Check for Normality Test for distribution of residuals
plot(fit, which = 2)
shapiro.test(fit$residuals)

# Check for equal variance / Heteroscedasticity
# Null Hyp is Homoscedasticity, <p rejects null. data is hetero
plot(fit, which = 3)

library(lmtest)
library(car)

bptest(fit)

ncvTest(fit)

# Adjust heteroscedastically corrected covariance matrix

coeftest(fit, vcov=hccm(fit))

# very similar to values for fit

# Check for Influencers
which.max(abs(fit$residuals))

rbind(data_train[c("182"),], data_train[c("387"),])

lev<-hat(model.matrix(fit))
summary(lev)

data_train[lev>0.04,]

influencePlot(fit)

require(MASS)
require("fit.models")
fmclass.add.class("lmfm", "rlm")
fm1<-fit.models(c("rlm", "lm"), y_train ~ L1+B2+T, data = data_train)
fm1

#It appears the identified “outlying” observations are not sufficiently different from 
#the underlying sample for us to remove them from our analysis.

# Check for autocorrelation
acf(fit$residual)

# Durbin - Watson Test in lmtest package to verify Autocorrection

dwtest(y_train ~ L1+B2+T, data = data_train)

# The p-value, at 0.57, indicates there is no evidence of correlated residuals. 
# If correlated, use gls() from nlme package

plot(fit$fitted, y_train, col = "blue")
abline(0, 1)

cor(fit$fitted, y_train)

# R2 statistic = 
cor(fit$fitted, y_train)^2 

# Test Performance
pred<-predict(fit, data_test)
cor(y_test, pred)
cor(y_test, pred)^2

plot(pred, y_test, col = "brown")
abline(0,1)

# Improving Model

# Feature Engineering
data_sample <- log (data.frame (data.matrix ( Handaxes [,3:8])))
data_sample$BB<- data_sample$B/data_sample$B1
data_sample$TT<- data_sample$T/data_sample$T1

data_sample$B<-NULL
data_sample$B1<-NULL
data_sample$T<-NULL
data_sample$T1<-NULL

head(data_sample)

y_train<-y[train]
y_test<-y[-train]
data_train<-data_sample[train,]
data_test<-data_sample[-train,]

fit1<- lm(y_train ~ ., data = data_train)
summary(fit1)

# R2 is higher than the first model
par(mfrow=c(2,2))
plot(fit1)

pred1<-predict(fit1, data_test)
cor(y_test, pred1)
cor(y_test, pred1)^2

plot(pred1, y_test, col = "brown")
abline(0,1)
