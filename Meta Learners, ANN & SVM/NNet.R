x<-cbind(hStudy=c(3,5,10), hSleep=c(5,1,2))
x<-x/max(x)
x
x_test<-cbind(hStudy=0.8, hSleep=0.3)
y<-cbind(score=c(75,82,93))
y<-y/100
y
y_test<-cbind(score=0.88)

input<-2
hidden<-3
output<-1

set.seed(1234)
W1<-matrix(data=rnorm(input*hidden), nrow=input, ncol=hidden)

set.seed(1234)
W2<-matrix(data=rnorm(hidden*output), nrow=hidden, ncol=output)

z2<-x %*% W1
a2<-sigmoid(z2)

z3<-a2 %*% W2
y_hat<-sigmoid(z3)
y_hat
y
myfunction<-function(x){
  return (exp(-x)/((1+exp(-x))^2))}

delta3<-(y-y_hat) * myfunction(z3)

dJdW2 <- t(a2) %*% delta3
dJdW2

delta2<-delta3 %*% t(W2) * myfunction(z2)

dJdW1<-t(x) %*% delta2
dJdW1

scalar = 5

W1+scalar*dJdW1
W1


#### 
data<-as.data.frame(cbind(x,y))
data
library(neuralnet)
set.seed(1234)
model <- neuralnet(formula = score ~ hStudy + hSleep,
                           data = data)
plot(model)
model_results <- compute(model, x_test)
predicted_strength <- model_results$net.result
predicted_strength
error <- (predicted_strength-y_test)^2
error
model2 <- neuralnet(formula = score ~ hStudy + hSleep, 
                   data = data, hidden = 3)
plot(model2)

model_results2 <- compute(model2, x_test)
predicted_strength2 <- model_results2$net.result
predicted_strength2
error2 <- (predicted_strength2-y_test)^2
error2
