
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


# 5


