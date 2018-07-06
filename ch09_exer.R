
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
  train_mistclass = c(train_mistclass, confusion_tab[-1, 1] + confusion_tab[1, -1])
}

par(mfrow=c(1,3))
plot(costs, cv_errors, type="b", col="red", log="x", main="CV error")
plot(costs, train_mistclass, type="b", col="blue", log="x", main="number of misclassified training data")
# Larger cost tend to lower value for both CV and training errors

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
  test_mistclass = c(test_mistclass, confusion_tab[-1, 1] + confusion_tab[1, -1])
}
plot(costs, test_mistclass, type="b", col="green", log="x", main="number of misclassified test data")
