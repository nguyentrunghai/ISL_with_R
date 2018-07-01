
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



# 9a
library(ISLR)
library(tree)
names(OJ)
train = sample(1:nrow(OJ), size=800, replace=FALSE)
test = -train

# 9b
tree.oj = tree(Purchase ~ ., data=OJ, subset=train)
summary(tree.oj)
# Only 3 out of 17 predictors were used in building the tree.
# There are 9 terminal nodes
# Training classification error rate is 0.17

# 9c
tree.oj
# If not sure how to interpret the printed text, look at page 325
# For example this termial node
# 18) LoyalCH < 0.205758 78   84.27 MM ( 0.23077 0.76923 ) *
# The test questions for an observation falling into this group can be obtained 
# by tracing back to the root,
# LoyalCH < 0.5036; LoyalCH < 0.276142; LoyalCH > 0.0356415; LoyalCH < 0.205758
# The overall prediction for this group is MM, the deviance is 84.27,
# The proportion of data points in this gorup having class MM is 0.76923

# 9d
plot(tree.oj)
text(tree.oj, pretty=0)
# The predictor LoyalCH is picked first to split. 
# all leaves on the right branch haves the same anwser: CH
# Although the majority on the right branch is already CH after the first split,
# the subsequent splits on the right are to increase node purity.
# See page 314 for explanation of the phenomenon.
# Four out of five leaves on the left branch give anwser MM

# 9e
tree.oj.pred = predict(tree.oj, newdata=OJ[test, ], type="class")
table(tree.oj.pred, OJ[test, "Purchase"])
# Test error rate 
(15 + 37) / length(tree.oj.pred) # 0.19

# 9f
set.seed(1)
cv.tree.oj = cv.tree(tree.oj, FUN=prune.misclass)

# 9g
plot(cv.tree.oj$size, cv.tree.oj$dev, type="b", xlab="size", ylab="CV error")
# why cv.tree.oj$dev is large??

# 9h
# size 4 and 9 are tied

# 9i
pruned.tree.oj = prune.misclass(tree.oj, best=4)

# 9j
pruned.tree.oj.pred = predict(pruned.tree.oj, newdata=OJ[test, ], type="class")
table(pruned.tree.oj.pred, OJ[test, "Purchase"])

# 9k
# Test error rate 
(15 + 37) / length(tree.oj.pred) # 0.19 same as unpruned tree



# 10a
library(ISLR)
Hitters_rmna = Hitters[ ! is.na(Hitters$Salary), ]
Hitters_rmna$Salary.log = log(Hitters_rmna$Salary)
Hitters_rmna$Salary = NULL

# 10b
train = 1:200
test = - train

# 10c
library(gbm)
lambda_grid = 10^seq(-6, 0, length=20)
train_mse = c()
set.seed(1)
for(lambda in lambda_grid)
{
  boost_mod = gbm(Salary.log ~ ., data=Hitters_rmna[train,], distribution="gaussian",
                  n.trees=1000, interaction.depth=4, shrinkage=lambda)
  yhat = predict(boost_mod, newdata=Hitters_rmna[train,], n.trees=1000)
  train_mse = c(train_mse, mean( (yhat - Hitters_rmna[train, "Salary.log"])^2 )  )
}

plot(lambda_grid, train_mse, type="b", log="x")


# 10d
test_mse = c()
set.seed(1)

for(lambda in lambda_grid)
{
  boost_mod = gbm(Salary.log ~ ., data=Hitters_rmna[train,], distribution="gaussian",
                  n.trees=1000, interaction.depth=4, shrinkage=lambda)
  yhat = predict(boost_mod, newdata=Hitters_rmna[test,], n.trees=1000)
  test_mse = c(test_mse, mean( (yhat - Hitters_rmna[test, "Salary.log"])^2 )  )
}

plot(lambda_grid, test_mse, type="b", log="x")
boost_best_lambda = lambda_grid[which.min(test_mse)]
boost_lowest_test_mse = min(test_mse)  # 0.2546597


# 10e
# multiple linear regression
lm_mod = lm(Salary.log ~ ., data=Hitters_rmna[train,])
yhat_lm = predict(lm_mod, newdata=Hitters_rmna[test,])
lm_test_mse = mean( (yhat_lm - Hitters_rmna[test, "Salary.log"])^2 )  # 0.491795

# ridge regression
library(glmnet)
grid = 10^seq(10, -2, length=100)
x = model.matrix(Salary.log ~ ., data=Hitters_rmna)
y = Hitters_rmna$Salary.log
ridge_mod = glmnet(x[train,], y[train], alpha=0, lambda=grid)   # alpha=0 ridge, alpha=1 lasso

ridge_test_mse = c()
for(lambda in grid)
{
  yhat_ridge = predict(ridge_mod, s=lambda, newx=x[test,])
  ridge_test_mse = c(ridge_test_mse, mean( (yhat_ridge - Hitters_rmna[test, "Salary.log"] )^2 ) )
}

plot(grid, ridge_test_mse, type="b", log="x")
ridge_best_lambda = grid[which.min(ridge_test_mse)]
rigde_lowest_mse = min(ridge_test_mse)    # 0.4426008

boost_lowest_test_mse  # 0.2546597
lm_test_mse            # 0.4917959
rigde_lowest_mse       # 0.4426008
# boosting gives significantly lower test mse than multiple linear regression and ridge regression

# 10f
boost_mod = gbm(Salary.log ~ ., data=Hitters_rmna[train,], distribution="gaussian",
                n.trees=1000, interaction.depth=4, shrinkage=boost_best_lambda)
summary(boost_mod)
# CHits, CAtBat and CWalks are the most important features.

# marginal effect of the three variables
par(mfrow=c(1,3))
plot(boost_mod, i="CHits")
plot(boost_mod, i="CAtBat")
plot(boost_mod, i="CWalks")

# initially, salary increases with increasing of these three variables.
# But when these three variables continue to increase, the salary becomes more or less flat.

# 10g
library(randomForest)
set.seed(1)
nvars = ncol(Hitters_rmna) - 1
bag_mod = randomForest(Salary.log ~ ., data=Hitters_rmna[train,], mtry=nvars, importance=TRUE)
yhat_bag = predict(bag_mod, newdata=Hitters_rmna[test,])
bag_test_mse = mean( (yhat_bag - Hitters_rmna[test, "Salary.log"])^2 )
bag_test_mse # 0.2301184
# baging gives slightly lower mse that boosting does.


# 11a
library(ISLR)
names(Caravan)
sum(is.na(Caravan))

train = 1:1000
test = -train

# gbm will complain if we dont change to 0 1 variable
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
# 11b
library(gbm)
boost_mod = gbm(Purchase ~ ., data=Caravan[train,], distribution="bernoulli",
                n.trees=1000, interaction.depth=4, shrinkage=0.01)
summary(boost_mod)
# PPERSAUT and MOPLHOOG seem to be the most important predictors

