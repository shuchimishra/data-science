#Simple Linear Regression with lm
x<-1:10
f<-function(i){y=0.5+3*(i)}
y<-f(x)
plot(y~x)
lines(y)

# install.packages("car")
library(car)
data(Quartet)
str(Quartet)
plot(Quartet$x, Quartet$y1)
lmfit <- lm(Quartet$y1~Quartet$x)
abline(lmfit, col="red")   
lmfit

coefficients(lmfit) # Extract model coefficients 
confint(lmfit, level = 0.95) # Computes confidence intervals for model parameters. 
fitted( lmfit) # Extract model fitted values 
residuals( lmfit) # Extract model residuals 
anova( lmfit) # Compute analysis of variance tables for fitted model object 
vcov( lmfit) # Calculate variance-covariance matrix for a fitted model object 
influence(lmfit) # Diagnose quality of regression fits

#Summarizing Linear Model Fits

summary(lmfit)

coefficients(lmfit)

#
model1<-lm(Ozone~Solar.R, data = airquality)
model1
summary(model1)
abline(model1)
coef(model1)
coefficients(model1)[1]
Ozone1=coefficients(model1)[1]+coefficients(model1)[2]*19
Ozone1
Ozone2=coefficients(model1)[1]+coefficients(model1)[2]*20
Ozone2
Ozone2-Ozone1
predict(model1, data.frame(Solar.R = 0))
Oz1= predict(model1, data.frame(Solar.R = 19))
Oz1
Oz2=predict(model1, data.frame(Solar.R = 20))
Oz2
Oz2-Oz1





#Using Linear Regression to Predict Unknown Values
plot(Quartet$x, Quartet$y1)
lmfit = lm( y1 ~ x, Quartet) 
abline(lmfit)
# Assign values to be predicted into newdata:
newdata = data.frame( x = c( 3,6,15)) 

# Compute the prediction result using the confidence interval with level set as 0.95:
predict( lmfit, newdata, interval ="confidence", level = 0.95) 

# Compute the prediction result using this prediction interval:
predict( lmfit, newdata, interval ="predict")
 

# Fitting a polynomial regression model with lm

plot(Quartet$x, Quartet$y2)
lmfit = lm(Quartet$y2~poly(Quartet$x,2))
lmfit2 = lm( Quartet $ y2 ~ I( Quartet$x) + I( Quartet $x^ 2))
par(mfrow=c(2,2))
lines(sort(Quartet$x), lmfit$fit[order(Quartet$x)], col = "red")
lines(sort(Quartet$x), lmfit2$fit[order(Quartet$x)], col = "blue")

#Robust Linear Regression With rlm 

library(MASS)
par(mfrow=c(1,2))
plot(Quartet$x, Quartet$y3)
lmfit = lm(Quartet$y3~Quartet$x)
abline(lmfit, col="blue")

plot(Quartet$x, Quartet$y3)
rlmfit = rlm(Quartet$y3~Quartet$x)
abline(rlmfit, col="red")
summary(rlmfit)
coefficients(lmfit)

#A Case Study of Linear Regression On SLID Data

library(car)
?SLID
data(SLID)
str(SLID)
attach(SLID)
par(mfrow=c(2,2))
plot(wages ~ language)
plot(wages ~ age)
plot(wages ~ education)
plot(wages ~ sex)
pairs.panels(SLID[c("language", "age", "education","sex")])
lmfit <- lm(wages ~ ., data = SLID)
summary(lmfit)
lmfit = lm(wages ~ age + sex + education, data = SLID)
summary(lmfit)
par(mfrow=c(2,2))
plot(lmfit)


#Gaussian Model For Generalized Linear Regression

lmfit1 = glm(wages ~ age + sex + education, data = SLID, family=gaussian)
summary(lmfit1)
lmfit2 = lm(wages ~ age + sex + education, data = SLID)
summary(lmfit2)


#Poisson Model For Generalized Linear Regression 

model.glm<-glm(Ozone~Solar.R, data = airquality, family=poisson)
model.glm
log (y) = a + bx + e
y = e^(a+bx) + e
y = e^a.e^bx + e
Oz1<-exp(coef(model.glm)[1]+coef(model.glm)[2]*19)
Oz1
Oz2<-exp(coef(model.glm)[1]+coef(model.glm)[2]*20)
Oz2
Oz2/Oz1
exp(coef(model.glm)[2])
######Ozone goes up my a factor of exponential of the coeff 
#for each unit change of Solar. This is a multiplicative change of the response variable
#change of 1 unit of Solar R, increase in Ozone concentration by e^0.003295 fold

#Binomial Model For Generalized Linear Regression 

data(mtcars)
head(mtcars$vs)
lm1 = glm(vs ~ hp+mpg+gear,data=mtcars, family=binomial)
summary(lm1)
