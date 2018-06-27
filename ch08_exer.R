
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



