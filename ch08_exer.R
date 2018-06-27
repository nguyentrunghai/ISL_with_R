
# Applied

# 7
set.seed(1)
library(MASS)
library(randomForest)
names(Boston)

train = sample(1:nrow(Boston), size=nrow(Boston)/2, replace = FALSE)
test = -train
p = ncol(Boston) - 1
ntrees = seq(1, 1000, by=100)

rf_test_error_cal = function(data, train, test, mtry, ntrees)
{
  errors = rep(NA, length(ntrees))
  
  for(i in seq_along(ntrees))
  {
    ntree = ntrees[i]
    rf = randomForest(medv ~ ., data=data, subset=train, 
                      mtry=mtry, ntree=ntree, importance=TRUE)
    yhat = predict(rf, newdata=data[test, ])
    errors[i] = mean((yhat - data[test, ]$medv)^2)
  }
  return(errors)
}

# mtry = 1
errors_1 = rf_test_error_cal(Boston, train, test, mtry=1, ntrees=ntrees)
# mtry = sqrt(p)
errors_2 = rf_test_error_cal(Boston, train, test, mtry=sqrt(p), ntrees=ntrees)
# mtry = p/2
errors_3 = rf_test_error_cal(Boston, train, test, mtry=p/2, ntrees=ntrees)
# mtry = p
errors_4 = rf_test_error_cal(Boston, train, test, mtry=p, ntrees=ntrees)

ymin = min( c(errors_1, errors_2, errors_3, errors_4) )
ymax = max(c(errors_1, errors_2, errors_3, errors_4))
plot(ntrees, errors_1, xlab="# of trees", ylab="testerror", type="l", col="red",
     ylim=c(ymin - 2 , ymax + 2))
lines(ntrees, errors_2, col="blue")
lines(ntrees, errors_3, col="green")
lines(ntrees, errors_4, col="orange")
legend("topright", legend=c("m=1", "m=sqrt(p)","m=p/2", "m=p"), 
       col=c("red", "blue", "green", "orange"), lty=1)



# 8a
library(ISLR)
names(Carseats)

train = sample(1:nrow(Carseats), size=nrow(Carseats)/2, replace=FALSE)
test = -train

#8b
library(tree)
tree.carseats = tree(Sales ~ ., data=Carseats, subset=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)
# the tree is unpruned, having 17 terminal nodes.
# The feature ShelveLoc is used first to split the predictor space, 
# (Bad, Medium) on the left branch and Good in the right
# Then Price is used to split both the left and right branches

tree.pred = predict(tree.carseats, newdata=Carseats[test, ])
tree.mse = mean( (tree.pred - Carseats[test, "Sales"] )^2 )
tree.mse # 4.602323


# 8c
set.seed(1)
cv.carseats = cv.tree(tree.carseats, FUN=prune.tree)
names(cv.carseats)
plot(cv.carseats$size, cv.carseats$dev, type="b")
cv.best.tree.size = cv.carseats$size[ which.min(cv.carseats$dev) ]
cv.best.tree.size  # 9

# test MSE of pruned tree
pruned.carseats = prune.tree(tree.carseats, best=cv.best.tree.size)
pruned.pred = predict(pruned.carseats, newdata=Carseats[test, ] )
pruned.mse = mean( (pruned.pred - Carseats[test, "Sales"] )^2 )
# 4.608781, a little worse test mse


# 8d
set.seed(1)
library(randomForest)
p = ncol(Carseats) - 1
bag.carseats = randomForest(Sales ~ ., data=Carseats, subset=train, mtry=p, Importance=TRUE)
bag.carseats
yhat.bag = predict(bag.carseats, newdata=Carseats[test, ])
mse.bag = mean( (yhat.bag - Carseats[test, "Sales"])^2 )
# 2.856059, about half of the mse given by regression tree
sort(importance(bag.carseats)[, 1], decreasing=TRUE)
# The most important feature is ShelveLoc, the second most is Price

# 8e
set.seed(1)
rf.carseats = randomForest(Sales ~ ., data=Carseats, subset=train, mtry=sqrt(p), Importance=TRUE)
rf.carseats
yhat.rf = predict(rf.carseats, newdata=Carseats[test,])
mse.rf = mean( (yhat.rf - Carseats[test, "Sales"])^2 )
# 3.162278 higher than bagging 
sort(importance(rf.carseats )[, 1], decreasing=TRUE)
# again ShelveLoc is the most important feature and Price is the second most.

# effect of mtry
test_err_vs_mtry = c()
for(mtry in 1:p)
{
  set.seed(1)
  rf.carseats = randomForest(Sales ~ ., data=Carseats, subset=train, mtry=mtry, Importance=TRUE)
  yhat.rf = predict(rf.carseats, newdata=Carseats[test,])
  mse.rf = mean( (yhat.rf - Carseats[test, "Sales"])^2 )
  test_err_vs_mtry = c(test_err_vs_mtry, mse.rf)
}
# it looks like bagging gives lower mse than random forest



