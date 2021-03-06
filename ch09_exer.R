
# Applied

# 4
library(e1071)

set.seed(2)
x = matrix( rnorm(100*2), ncol=2)
y = c(rep(0, 50), rep(1, 50))
x[51:75] = x[51:75] + 3
x[76:100] = x[76:100] - 3
plot(x, col=y+1)
data = data.frame( x=x, y=as.factor(y) )

train = sample(1:100, size=50, replace=FALSE)
test = -train

cost = c(0.1, 1, 10, 100, 1000)
gamma = c(0.5, 1, 2, 3, 4)

# polynomial degree 1
svmfit_deg1 = tune(svm, y ~ ., data=data[train, ], kernel="polynomial", degree=1, 
                   scale=FALSE, decision.values=T,
                   range=list(cost=cost) )$best.model

table(pred_train = predict(svmfit_deg1, newdata=data[train, ]), truth_train=data[train, ]$y)
#           truth_train
# pred_train  0  1
#          0  0  0
#          1 22 28
table(pred_test = predict(svmfit_deg1, newdata=data[test, ]), truth_test=data[test, ]$y)
#          truth_test
# pred_test  0  1
#         0  0  0
#         1 28 22

# polynomial degree 2
svmfit_deg2 = tune(svm, y ~ ., data=data[train, ], kernel="polynomial", degree=2, 
                   scale=FALSE, decision.values=T,
                   range=list(cost=cost) )$best.model

table(pred_train = predict(svmfit_deg2, newdata=data[train, ]), truth_train=data[train, ]$y)
#            truth_train
# pred_train  0  1
#          0 18  4
#          1  4 24
table(pred_test = predict(svmfit_deg2, newdata=data[test, ]), truth_test=data[test, ]$y)
#          truth_test
# pred_test  0  1
#          0 22  4
#          1  6 18


# polynomial degree 3
svmfit_deg3 = tune(svm, y ~ ., data=data[train, ], kernel="polynomial", degree=3, 
                   scale=FALSE, decision.values=T,
                   range=list(cost=cost) )$best.model

table(pred_train = predict(svmfit_deg3, newdata=data[train, ]), truth_train=data[train, ]$y)
#           truth_train
# pred_train  0  1
#          0  2  0
#          1 20 28
table(pred_test = predict(svmfit_deg3, newdata=data[test, ]), truth_test=data[test, ]$y)
#          truth_test
# pred_test  0  1
#         0  2  1
#         1 26 21


# polynomial degree 4
svmfit_deg4 = tune(svm, y ~ ., data=data[train, ], kernel="polynomial", degree=4, 
                   scale=FALSE, decision.values=T,
                   range=list(cost=cost) )$best.model

table(pred_train = predict(svmfit_deg4, newdata=data[train, ]), truth_train=data[train, ]$y)
#           truth_train
# pred_train  0  1
#           0 22  1
#           1 0 27
table(pred_test = predict(svmfit_deg4, newdata=data[test, ]), truth_test=data[test, ]$y)
#          truth_test
# pred_test  0  1
#         0 23  6
#         1  5 16

# polynomial kernels with odd degrees give very bad results, interesting!

# radial
svmfit_radial = tune(svm, y ~ ., data=data[train, ], kernel="radial", 
                     scale=FALSE, decision.values=T,
                   range=list(cost=cost, gamma=gamma) )$best.model

table(pred_train = predict(svmfit_radial, newdata=data[train, ]), truth_train=data[train, ]$y)
#           truth_train
# pred_train  0  1
#          0 22  0
#          1  0 28
table(pred_test = predict(svmfit_radial, newdata=data[test, ]), truth_test=data[test, ]$y)
#          truth_test
# pred_test  0  1
#         0 20  5
#        1  8 17


# ROC curves
library(ROCR)
rocplot = function(pred, truth, ...)
{
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit_deg1_scores = attributes( predict(svmfit_deg1, newdata=data[test, ], 
                                         decision.values=TRUE) )$decision.values

svmfit_deg2_scores = attributes( predict(svmfit_deg2, newdata=data[test, ], 
                                         decision.values=TRUE) )$decision.values

svmfit_radial_scores = attributes( predict(svmfit_radial, newdata=data[test, ], 
                                         decision.values=TRUE) )$decision.values

rocplot(svmfit_deg1_scores, data$y[test], col="red", main="ROC curves")
rocplot(svmfit_deg2_scores, data$y[test], add=TRUE, col="blue")
rocplot(svmfit_radial_scores, data$y[test], add=TRUE, col="green")
legend("bottomright", legend = c("linear", "polyn. deg. 2", "radial"), 
       col=c("red", "blue", "green"), lty=1)

# so 2nd degree polynomial kernel gives the best ROC curve
plot(svmfit_deg2, data[test, ])



# 5a
set.seed(1)
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1*(x1^2 + x2^2 > 0.3^2)    # circular disk
data = data.frame(x1=x1, x2=x2, y=as.factor(y))

# 5b
par(mfrow=c(3,2))
x = cbind(x1, x2)
plot(x, col=y+1, main="data")

# 5c
logistic_mod = glm(y ~ ., data=data, family=binomial)
summary(logistic_mod)

# 5d
lin_prob = predict(logistic_mod, newdata=data, type="response")
lin_yhat = ifelse(lin_prob > 0.5, 1, 0)
plot(x, col=lin_yhat+1, main="predicted by logistic reg.")

# 5e
nonlin_logistic_mod = glm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2), data=data, family=binomial)

# 5f
nonlin_prob = predict(nonlin_logistic_mod, newdata=data, type="response")
nonlin_yhat = ifelse(nonlin_prob > 0.5, 1, 0)
plot(x, col=nonlin_yhat+1, main="predicted by logistic reg. with nonlin. transf.")

# 5g
library(e1071)
cost = c(0.1, 1, 10, 100, 1000)
gamma = c(0.5, 1, 2, 3, 4)
lin_svm_mod = tune(svm, y ~ ., data=data, kernel="linear",
                   scale=FALSE, decision.values=T,
                   range=list(cost=cost) )$best.model
lin_svm_yhat = as.numeric( predict(lin_svm_mod, newdata=data) ) -1
plot(x, col=lin_svm_yhat+1, main="predicted by SVM with linear kernel.")

# 5h
nonlin_svm_mod = tune(svm, y ~ ., data=data, kernel="radial", 
                   scale=FALSE, decision.values=T,
                   range=list(cost=cost, gamma=gamma) )$best.model
nonlin_svm_yhat = as.numeric( predict(nonlin_svm_mod, newdata=data) ) - 1
plot(x, col=nonlin_svm_yhat+1, main="predicted by SVM with nonlinear kernel.")


# 6a

set.seed(2)
x1 = rnorm(200)
x2 = rnorm(200)
y = c( rep(-1, 100), rep(1, 100) )
x1[y==1] = x1[y==1] + 2.
x2[y==1] = x2[y==1] + 2.

x = cbind(x1, x2)
plot(x, col=(3-y))

data = data.frame(x1=x1, x2=x2, y=as.factor(y))

# 6b
set.seed(1)
library(e1071)
costs = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
tune_lin_svm = tune(svm, y ~ ., data=data, kernel="linear", scale=FALSE, decision.values=TRUE, 
                    range=list(cost=costs))

summary(tune_lin_svm)
cv_errors = summary(tune_lin_svm)$performances[, 2]
# cost = 0.01 gives the lowest CV error, but larger costs do not make the error a lot worse


train_mistclass = c()
for(cost in costs)
{
  lin_svm_mod = svm(y ~ ., data=data, kernel="linear", cost=cost, scale=F, decision.values=T)
  pred = predict(lin_svm_mod, newdata=data)
  confusion_tab = table(pred=pred, truth=data$y)
  train_mistclass = c(train_mistclass, (confusion_tab[-1, 1] + confusion_tab[1, -1])/sum(confusion_tab) )
}

par(mfrow=c(1,3))
plot(costs, cv_errors, type="b", col="red", log="x", main="CV error")
plot(costs, train_mistclass, type="b", col="blue", log="x", main="train error rate")


# 6c
set.seed(10)
x1_test = rnorm(200)
x2_test = rnorm(200)
y_test = c( rep(-1, 100), rep(1, 100) )
x1_test[y_test==1] = x1_test[y_test==1] + 2.
x2_test[y_test==1] = x2_test[y_test==1] + 2.

data_test = data.frame(x1=x1_test, x2=x2_test, y=as.factor(y_test))

test_mistclass = c()
for(cost in costs)
{
  lin_svm_mod = svm(y ~ ., data=data, kernel="linear", cost=cost, scale=F, decision.values=T)
  pred = predict(lin_svm_mod, newdata=data_test)
  confusion_tab = table(pred=pred, truth=data_test$y)
  test_mistclass = c(test_mistclass, (confusion_tab[-1, 1] + confusion_tab[1, -1])/sum(confusion_tab) )
}
plot(costs, test_mistclass, type="b", col="green", log="x", main="test error rate")

# 6d
# The CV error has minimun at cost=0.01 and again at large costs of 100, 1000, 10000.
# So the trend of CV error is not very clear.
# As expected the larger the cost is the lower the training error becomes.
# The test error rate has minimun at cost 0.01 and 0.1.


# 7a
library(ISLR)
mpg_meadian = median(Auto$mpg)
mpg_high = 1*(Auto$mpg > mpg_meadian)
Auto$mpg_high = as.factor(mpg_high)
Auto$mpg = NULL

Auto$origin = as.factor(Auto$origin)

Auto_scaled = Auto
predictors = names(Auto)
predictors = predictors[predictors != "mpg_high"]
for(predictor in predictors)
{
  if( class(Auto_scaled[, predictor]) == "numeric" )
  {
    Auto_scaled[, predictor] = scale(Auto_scaled[, predictor])
  }
}

# 7b
set.seed(1)
library(e1071)
costs = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)

tune_lin_svm = tune(svm, mpg_high ~ ., data=Auto_scaled, kernel="linear", scale=FALSE, 
                    range=list(cost=costs))
cv_errors_lin = summary(tune_lin_svm)$performances
cv_errors_lin
plot(costs, cv_errors_lin[,2], type="b", log="x", main="SVM with linear kernel")
# cost=0.01 and cost=1 give lowest CV error rate: 0.08673077

# 7c
gammas = c(0.1, 0.5, 1, 2, 3, 4, 5)
tune_radial_svm = tune(svm, mpg_high ~ ., data=Auto_scaled, kernel="radial", scale=FALSE, 
                    range=list(cost=costs, gamma=gammas) )
cv_errors_radial = summary(tune_radial_svm)$performances
cv_errors_radial[ which.min(cv_errors_radial$error), ]
# cost=1, gamma=0.5 gives the lowest CV error rate: 0.07397436 which is smaller than SVM with linear kernel

# 7d
lin_svm_best = tune_lin_svm$best.model
plot(lin_svm_best, Auto_scaled, displacement ~ horsepower)
plot(lin_svm_best, Auto_scaled, horsepower ~ weight)
plot(lin_svm_best, Auto_scaled, horsepower ~ acceleration)
plot(lin_svm_best, Auto_scaled, weight ~ acceleration)

radial_svm_best = tune_radial_svm$best.model
plot(radial_svm_best, Auto_scaled, displacement ~ horsepower)
plot(radial_svm_best, Auto_scaled, horsepower ~ weight)
plot(radial_svm_best, Auto_scaled, horsepower ~ acceleration)
plot(radial_svm_best, Auto_scaled, weight ~ acceleration)
# not so easy to imterpret these plots which are projection on 2d planes


# 8a
library(ISLR)
names(OJ)
mean(OJ$Purchase == "CH")  # 0.61 rather balanced dataset

train = sample(1:nrow(OJ), size=800, replace=FALSE)
test = -train

# 8b
library(e1071)
svm_lin_cost0.01 = svm(Purchase ~ ., data=OJ[train,], kernel="linear", scale=TRUE, cost=0.01)
summary(svm_lin_cost0.01)

# 8c
error_rate = function(model, data, subset, response)
{
  pred = predict(model, newdata=data[subset,])
  confus_mat = table(pred=pred, truth=data[subset, response])
  error = (confus_mat[1, 2] + confus_mat[2, 1]) / sum(confus_mat)
  return(error)
}

train_error_lin_cost0.01 = error_rate(svm_lin_cost0.01, OJ, train, "Purchase")
train_error_lin_cost0.01 # 0.17

test_error_lin_cost0.01 = error_rate(svm_lin_cost0.01, OJ, test, "Purchase")
test_error_lin_cost0.01  # 0.1740741


# 8d
set.seed(1)
costs = 10^seq(-2, 1, length=10)
tune_lin = tune(svm, Purchase ~ ., data=OJ[train,], kernel="linear", scale=TRUE,
                         range=list(cost=costs))
cv_error_lin = summary(tune_lin)$performances
cv_error_lin[ which.min(cv_error_lin$error), ]
# cost = 0.464 gives lowest CV error: 0.1675

svm_lin_best = tune_lin$best.model

train_error_lin_best = error_rate(svm_lin_best, OJ, train, "Purchase")
train_error_lin_best # 0.165

test_error_lin_best = error_rate(svm_lin_best, OJ, test, "Purchase")
test_error_lin_best  # 0.1703704


#8e
svm_radial_cost0.01 = svm(Purchase ~ ., data=OJ[train,], kernel="radial", scale=TRUE, cost=0.01)
summary(svm_radial_cost0.01)

train_error_radial_cost0.01 = error_rate(svm_radial_cost0.01, OJ, train, "Purchase")
train_error_radial_cost0.01 # 0.3975

test_error_radial_cost0.01 = error_rate(svm_radial_cost0.01, OJ, test, "Purchase")
test_error_radial_cost0.01 #  0.3666667


set.seed(1)
tune_radial = tune(svm, Purchase ~ ., data=OJ[train,], kernel="radial", scale=TRUE,
                range=list(cost=costs))
cv_error_radial = summary(tune_radial)$performances
cv_error_radial[ which.min(cv_error_radial$error), ]
# cost = 4.641589 gives lowest cv error rate: 0.18

svm_radial_best = tune_radial$best.model

train_error_radial_best = error_rate(svm_radial_best, OJ, train, "Purchase")
train_error_radial_best  # 0.15125

test_error_radial_best = error_rate(svm_radial_best, OJ, test, "Purchase")
test_error_radial_best  # 0.162963


# 8d
svm_polyn_cost0.01 = svm(Purchase ~ ., data=OJ[train,], kernel="polynomial", degree=2, scale=TRUE, cost=0.01)
summary(svm_polyn_cost0.01)

train_error_polyn_cost0.01 = error_rate(svm_polyn_cost0.01, OJ, train, "Purchase")
train_error_polyn_cost0.01 # 0.39625

test_error_polyn_cost0.01 = error_rate(svm_polyn_cost0.01, OJ, test, "Purchase")
test_error_polyn_cost0.01 # 0.362963


set.seed(1)
tune_polyn = tune(svm, Purchase ~ ., data=OJ[train,], kernel="polynomial", degree=2, scale=TRUE,
                   range=list(cost=costs))
cv_error_polyn = summary(tune_polyn)$performances
cv_error_polyn[ which.min(cv_error_polyn$error), ]
# cost = 10 gives lowest cv error rate: 0.1775

svm_polyn_best = tune_polyn$best.model

train_error_polyn_best = error_rate(svm_polyn_best, OJ, train, "Purchase")
train_error_polyn_best  # 0.155

test_error_polyn_best = error_rate(svm_polyn_best, OJ, test, "Purchase")
test_error_polyn_best  # 0.162963

# 8e
test_error_lin_best  # 0.1703704
test_error_radial_best  # 0.162963
test_error_polyn_best  # 0.162963




