
# 5.4 Exercises

# Conceptual

# 1 See figure Figure_ch05_exer_1.pdf


# 2a
# The probability that the j-th observation is selected as the first bootstrap observation is 1/n.
# So the probability that it is not selected as the first bootstrap observation is 1 - 1/n

# 2b
# 1 - 1/n. Bootstrap observations are drawn with replacement from the original dataset, 
# so they are independent and identically distributed.

# 2c
# The j-th observation not being in the bootstrap sample means that it is not selected as the first bootstrap observation
# AND not selected as the second bootstrap observation AND so on ... AND, finally, not selected as the n-th bootstrap observation.
# These n events are independent and have the same probablity 1 - 1/n. 
# So the probability that all of them happen simultaneously is (1 - 1/n)^n.

# 2d 
1 - (1 - 1/5)^5   # 0.67232

# 2e
1 - (1 - 1/100)^100  # 0.6339677

# 2f 
1 - (1 - 1/10000)^10000   # 0.632139

# 2g
n = 1:100000
p = 1 - (1 - 1/n)^n
plot(n, p, type="l")
# when n increases, p approaches a constant value around 0.632

# 2h
store=rep(NA, 10000)
for(i in 1:10000)
{
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0
}
mean(store)
# The proportion of bootstrap samples that contain 4 is 0.641 which is very close to 0.632



# 3a
# The original dataset of size n is randomly partitioned into 
# k mutually exclusive and collectively exhaustive subsets of roughly the same size
# For each subset, fit the model to all the other k-1 subsets and calculate test MSE or 
# error rate using the subset being considered as a test set.
# Average the test MSE or error rate over k.

# 3b
# Compare to validation set approach the k-fold CV is more computationally expensive
# but it has lower variance in estimating the test MSE or error rate.
# It is also more accurate because it makes better use of the dataset to train the model.

# Compare to LOOCV, k-fold CV is less expensive becuase we need to fit the model only k times
# rather than n times as in LOOCV, althoug for least squares linear or polynomial regression, 
# there is a exact formula for LOOCV MSE.
# k-fold CV use less data for each fit, so it has larger bias than LOOCV. 
# On the other hand, each fit in LOOCV is almost the same, so it produces more variance in 
# the error estimate than k-fold CV does.
# For least squares linear or polynomial regression, there is a exact formula for LOOCV MSE.


# 4
# We can use the bootstrap. 
# First randomly draw a bootstrap sample with replacement from the original training set. 
# We fit the model to the bootstrap training set.
# Then we use the model to predict Y from X.
# If we repeat the process above many times, we will get different values of Y.
# Finally, we can estimate the standard deviation of Y over many bootstrap samples.


# Applied

# 5a
library(ISLR)
set.seed(1)
logistic_fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(logistic_fit)

# 5b
# i
nsamples = nrow(Default)
train = rep(FALSE, nsamples)
train[1:nsamples/2] = TRUE

set.seed(1)
train = sample(train)
validation = !train

# ii
logistic_fit = glm(default ~ income + balance, data=Default, subset = train, family = binomial)

# iii
logistic_posterior = predict(logistic_fit, Default[validation,], type="response")
logistic_pre = rep("No", length(logistic_posterior))
logistic_pre[logistic_posterior > 0.5] = "Yes"
logistic_pre = factor(logistic_pre)

# iv
confusion_matrix = table(Default[validation, "default"], logistic_pre)
# validation error rate
1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
# 2.66 %

# 5c
for (i in 1:3)
{
  train = sample(train)
  validation = !train
  logistic_fit = glm(default ~ income + balance, data=Default, subset = train, family = binomial)
  
  logistic_posterior = predict(logistic_fit, Default[validation,], type="response")
  logistic_pre = rep("No", length(logistic_posterior))
  logistic_pre[logistic_posterior > 0.5] = "Yes"
  logistic_pre = factor(logistic_pre)
  
  confusion_matrix = table(Default[validation, "default"], logistic_pre)
  error_rate = 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(paste("Repeat", i, ", Validation error rate:", error_rate*100, "%"))
}
# The validation error rate changes quit a bit with different splits of the data
# into train and validation sets


# 5d
set.seed(1)
train = sample(train)
validation = !train

logistic_fit = glm(default ~ income + balance + student, data=Default, subset = train, family = binomial)

logistic_posterior = predict(logistic_fit, Default[validation,], type="response")
logistic_pre = rep("No", length(logistic_posterior))
logistic_pre[logistic_posterior > 0.5] = "Yes"
logistic_pre = factor(logistic_pre)

confusion_matrix = table(Default[validation, "default"], logistic_pre)
# validation error rate
1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
# 2.56%
# Including student reduces just a little bit the validation error rate


# 6a
set.seed(1)
logistic_fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(logistic_fit)$coefficients

# 6b
boot.fn = function(data, index)
{
  coefficients(glm(default ~ income + balance, data = data, subset = index, family = binomial))
}

# 6c
set.seed(1)
library(boot)
boot(Default, boot.fn, 1000)

# 6d
# The two standard error estimates are very similar.


# 7a
set.seed(1)
logistic_fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(logistic_fit)

# 7b
set.seed(1)
logistic_fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, subset = -1, family = binomial)
summary(logistic_fit)

set.seed(1)
logistic_fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
summary(logistic_fit)
# Same, so the syntax "subset = -1" is right.

# 7c
logistic_posterior = predict(logistic_fit, Weekly[1,], type="response")
logistic_pre = "Down"
if (logistic_posterior > 0.5) logistic_pre = "Up"
logistic_pre == Weekly[1,]$Direction 
# So logistic regression prediction is wrong.

# 7d
set.seed(1)
n = nrow(Weekly)
correct_predictions = rep(FALSE, n)

for (i in 1:n)
{
  logistic_fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
  logistic_posterior = predict(logistic_fit, Weekly[i,], type = "response")
  
  logistic_pre = "Down"
  if (logistic_posterior > 0.5) logistic_pre = "Up"
  correct_predictions[i] = (logistic_pre == Weekly[i,]$Direction)
}

# 7e
1 - mean(correct_predictions)
# LOOCV error rate is 45%


# 8a
set.seed(1)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)
# p = 2, n = 100
# Y = X - 2X^2 + e,
# where X ~ Normal(0, 1), e ~ Nornam(0, 1)

# 8b
plot(x, y)
# There is a nonlinear association between X and Y

# 8c
data = data.frame(x, y)
set.seed(1)

# i
linear_fit = glm(y ~ x, data = data)
cv.glm(data, linear_fit)$delta
# 7.288162 7.284744

# ii
square_fit = glm(y ~ x + I(x^2), data = data)
cv.glm(data, square_fit)$delta
# 0.9374236 0.9371789

# iii
cubic_fit = glm(y ~ x + I(x^2) + I(x^3), data = data)
cv.glm(data, cubic_fit)$delta
# 0.9566218 0.9562538

# iv
fourth_fit = glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = data)
cv.glm(data, fourth_fit)$delta
# 0.9539049 0.9534453
# The fit improves dramatically from linear to square model, but shows no improvement beyond that.

# 8d
set.seed(231)

linear_fit = glm(y ~ x, data = data)
cv.glm(data, linear_fit)$delta
# 7.288162 7.284744

square_fit = glm(y ~ x + I(x^2), data = data)
cv.glm(data, square_fit)$delta
# 0.9374236 0.9371789

cubic_fit = glm(y ~ x + I(x^2) + I(x^3), data = data)
cv.glm(data, cubic_fit)$delta
# 0.9566218 0.9562538

fourth_fit = glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = data)
cv.glm(data, fourth_fit)$delta
# 0.9539049 0.9534453

# Of course, LOOCV error rate does not depend on the random seed.
# Every time, the method just makes the same loop over every data point.

# 8e
# The square model gives the lowest LOOCV error rate which is expected because  
# the data was simulated from a 2nd order polynormial model.

# 8f
summary(linear_fit)
summary(square_fit)
summary(cubic_fit)
summary(fourth_fit)
# Only coefficients for x and x^2 are statistically significant. 
# This is consistent with the conclusion from LOOCV analysis that
# the 2nd order polynomial is the best model.


