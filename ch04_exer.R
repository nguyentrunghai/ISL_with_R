
# 4.7 Exercises
# Conceptual

# 1. See Figure_ch04_exer_1.pdf

# 2. See Figure_ch04_exer_2.pdf

# 3. See Figure_ch04_exer_3.pdf

# 4a 
# 10 %. 
# On Average 10% of the observations randomly drawn from Uniform(0, 1) will fall within [0.55, 0.65]. 
(0.65 - 0.55) / (1 - 0)  # 10 %

# 4b
# Call A the event that X1 is in [0.55, 0.65], P(A) = 0.1
# Call B the event that X2 is in [0.30, 0.40], P(B) = 0.1
# So the event that both A and B happen has the probability of P(A and B) = P(A) * P(B) = 
0.1 * 0.1 # 0.01  only 1%

# 4c
0.1 ^ 100 # very small

# 4d
# When p, the dimension of the input space, increases, fewer and fewer training data points are close to 
# any particular test predictor.

# 4e.
# Call L the length of each dimension of the hypercube centered at the test point.
# The Volume of this hypercube is L^p.
# The total volume of the input space is (1 - 0)^p = 1.
# The fraction of training data points within the hypercube is L^p / 1 = L^p.
# Keeping this fraction fixed at 10% means that L^p = 0.1
# Or L = (0.1) ^ (1/p)
(0.1) ^ (1/1)   # 0.1
(0.1) ^ (1/2)   # 0.3162278
(0.1) ^ (1/10)  # 0.7943282
(0.1) ^ (1/100) # 0.9772372
# So when p increases, L approaches 1


# 5a 
# We always expect a more flexible method (in this case, QDA) to fit the training set better 
# than a less flexible one (LDA).
# However, if the true boundary is very close to linear we expect the linear model (LDA) to 
# give lower test error rate than the quadratic model (QDA) which may overfit the training data.

# 5b
# As above we expect QDA to perform better than LDA on the test set.
# Since the true boundary is nonlinear, and fitting a linear model may create a large bias, we expect 
# the QDA to perform better becuase it may reduce siginicantly the bias.

# 5c
# In general, as n increases, we expect the test accuracy of QDA relative to LDA to improve.
# The reason is that QDA create less bias than LDA but may have large variance if the sample size is samll.
# We can reduce the variance of QDA by increasing the sample size to the point that it small enough that
# the overal test error which is sum of bias and variance is smaller te test error of LDA.

# 5d
# False. 
# The problem is that QDA or any method does not know what the true answer is.
# What the mehod is presented with is just a set of training data. And the more flexible the method is,
# the more it follows noise in the training data too closely and picks up patterns that is nonexistent
# in the test data. 


# 6a
probability = function(beta_0, betas, Xs){
  exponent = beta_0 + sum(betas * Xs)
  p = exp(exponent) / (1 + exp(exponent))
  return(p)
}

beta_0 = -6
betas = c(0.05, 1.)
Xs = c(40, 3.5)
probability(beta_0, betas, Xs)     # 0.3775407

# 6b 
# By definition: logit =  log( p / (1-p) )
# By logistic model: logit = beta_0 + beta_1*X1 + beta_2*X2
# So X1 = (logit - beta_0 - beta_2*X2) / beta_1
p = 0.5
logit = log( p / (1 - p) )   # 0
X1 = (logit - beta_0 - betas[2]*Xs[2]) / betas[1]   # need to study 50 hours which is 10 hours more.


# 7
# Use the linear discriminant function (4.12) to decide.
# This corresponds to using the threshod 0.5 for the Bayesian posterior.
# If we want to use other threshold, we have to calculate the posterior probability
# from the Bayes' rule.
lda_func = function(x, m, s, prior){
  lda_val = x*m/(s^2) - (m^2)/2/(s^2) + log(prior)
  return(lda_val)
}

s = 6  # standard deviation, not variance
x = 4
# lda for Yes class
m_Yes = 10
prior_Yes = 0.8
lda_func(x, m_Yes, s, prior_Yes)   # -0.5009213

# lda for No class
m_No = 0
prior_No = 0.2
lda_func(x, m_No, s, prior_No)    # -1.609438
# so the prediction is Yes, the compay will issue divident

# Use Bayes' theorem
normal_den = function(x, m, s){
  den = 1/(sqrt(2*pi) * s) * exp( (-(x - m)^2) / (2*(s^2)) )
  return(den)
}

unnormalized_posterior = function(x, m, s, prior){
  likelihood = normal_den(x, m, s)
  posterior = prior * likelihood
  return(posterior)
}

# for Yes class
unnormalized_posterior(x, m_Yes, s, prior_Yes)   # 0.03226276
# for No class
unnormalized_posterior(x, m_No, s, prior_No)    # 0.01064827

# normalized posterior for Yes:
0.03226276 / (0.03226276 + 0.01064827)     # 0.752
# normalized posterior for No:
0.01064827 / (0.03226276 + 0.01064827)     # 0.248

# so we will predict Yes with any threshold at most 0.75


# 8
# KNN with K=1 will give training error of 0 %. So An average error of 18 % 
# means the test error rate is 36 % which is worse than logistic regression with 
# the test error of 30 %.


# 9
# odds = p / (1 - p), 
# and so, p = odds / (1 + odds)

odds = function(p){
  return(p / (1 - p))
}

p = function(odds){
  return(odds / (1 + odds))
}

# 9a 
p(0.37)   # 0.270073

# 9b
odds(0.16)  # 0.1904762


# Applied

# 10
library(ISLR)

# 10a
names(Weekly)
?Weekly      # Direction is qualitative variable with two levels "Up" and "Down"
summary(Weekly)  # there are 605 weeks when the stock went up and 484 when it went down
cor(Weekly[,-9])
plot(Weekly)  # The only noticeable association is between Year and Volume.
              # The latter the Year was the more stocks were traded.


par(mfrow=c(2,4))
for (n in names(Weekly)){
  if (n != "Direction"){
    plot(Weekly[, "Direction"], Weekly[, n], xlab="Direction", ylab=n)
  }
}
# There is no noticeable association between Direction the remaining variables either,
# except between Direction and Today, which are the same thing: when Today is negative 
# Direction is Down and vice versa.

# 10b
logistic_fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(logistic_fit)
# The p-value of Lag2 (3%) is less than 5%, so we can say that it is statistically significant.
# Other predictors have p-values at least 11% and are not statistically significant.

# 10c
contrasts(Weekly$Direction)   # Up is coded as 1. So the probability is for Up
# type = "response" means output probabilities of the form P(Y=1|X).
logistic_probs = predict(logistic_fit, type="response")
logistic_pred = rep("Down", length(logistic_probs))
logistic_pred[logistic_probs > 0.5] = "Up"
Direction_true = Weekly$Direction

# the confusion matrix
confusion = table(Direction_true, logistic_pred)

# overall fraction of correct prediction
sum(diag(confusion)) / sum(confusion)    
# 56.1 %. Not so great.
# There are 55.5% of the weeks that saw stock went up.
# A stupid classifier that assigns every week to "Up" can achieve a similar correct prediction rate.

# Rows the confusion matrix correspond to the true Direction.
# The columns correspond to Direction predicted by logistic regression model.
# The diagonal elements are number of weeks whose Direction are predicted correctly.
# The off-diagonal elements are number of weeks whose Direction are predicted incorrectly.

# True positive rate
confusion["Up", "Up"] / (confusion["Up", "Down"] + confusion["Up", "Up"])
# 92 % is a high True pos. rate. 
# Among the weeks that saw the stock went up, 92% are predicted correctly by the logistic regression model.

# False positive rate
confusion["Down", "Up"] / (confusion["Down", "Up"] + confusion["Down", "Down"])
# 89% also a high False positive rate.
# Among the weeks that saw the stock went down, 89% are predicted wrongly by the logistic regression model.
# So this classifier put too many weeks into the "Up" bucket.

# Precision
confusion["Up", "Up"] / (confusion["Down", "Up"] + confusion["Up", "Up"])
# 56.4 %
# Among the weeks that are predicted to go up, 56% of them actually went up.

# 10d
train = (Weekly$Year < 2009)
test = !train
Weekly_after2008 = Weekly[test,]
Direction_true_after2008 = Direction_true[test]

logistic_fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(logistic_fit)
# p-value of Lag2 is 4%, still statistically significant.

logistic_probs = predict(logistic_fit, Weekly_after2008, type="response")
logistic_pred = rep("Down", length(logistic_probs))
logistic_pred[logistic_probs > 0.5] = "Up"

confusion = table(Direction_true_after2008, logistic_pred)
# overall fraction of correct prediction
sum(diag(confusion)) / sum(confusion) 
# 62.5 %, so the test error rate is 37.5 %

# 10e
library(MASS)
lda_fit = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda_pred = predict(lda_fit, Weekly_after2008)$class
confusion = table(Direction_true_after2008, lda_pred)
confusion
# overall fraction of correct prediction
sum(diag(confusion)) / sum(confusion) 
# 0.625 %, the error rate is 37.5 %
# Same as logistic regression

# 10f
qda_fit = qda(Direction ~ Lag2, data=Weekly, subset=train)
qda_pred = predict(qda_fit, Weekly_after2008)$class
confusion = table(Direction_true_after2008, qda_pred)
confusion
# all weeks are predicted to go up!

# overall fraction of correct prediction
sum(diag(confusion)) / sum(confusion)
# 58.6 % the same as the fracetion of "Up" in the test set:
mean(Direction_true_after2008 == "Up")
# test error rate is 41.4 %

# 10g
library(class)
train.X = scale( Weekly[train,]$Lag2 )
train.Y = Weekly[train,]$Direction

test.X = scale( Weekly[test,]$Lag2 )

set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=1)
confusion = table(Direction_true_after2008, knn_pred)

# overall fraction of correct prediction
sum(diag(confusion)) / sum(confusion)
# 49. %, the error rate is 51. %, like random guessing!

# 10h
# test error rates
# logistic regression: 37.5 %
# LDA:                 37.5 %
# QDA:                 41.4 %
# KNN with k=1:        51.0 %
# So logistic regression and lda appear to perform the best

# 10i
do_classify = function(method, data, predictors, 
                       response, train, test,
                       k=1){
  data$Y = data[, response]
  variable_names = c(predictors, "Y")
  
  if (method == "logistic"){
    model_fit = glm(Y ~ ., data=data[, variable_names], family=binomial, subset=train)
    
    probs = predict(model_fit, data[test, variable_names], type="response")
    pred = rep("Down", length(probs))
    pred[probs > 0.5] = "Up"
  }
  
  if (method == "lda"){
    model_fit = lda(Y ~ ., data=data[, variable_names], subset=train)
    pred = predict(model_fit, data[test, variable_names])$class
  }
  
  if (method == "qda"){
    model_fit = qda(Y ~ ., data=data[, variable_names], subset=train)
    pred = predict(model_fit, data[test, variable_names])$class
  }
  
  if (method == "knn"){
    train.X = scale( data[train, predictors] )
    train.Y = data[train,]$Y
    test.X = scale( data[test, predictors] )
    pred = knn(train.X, test.X, train.Y, k=k)
  }
  
  confus = table(data$Y[test], pred)
  error_rate = 1 - sum(diag(confus)) / sum(confus)
  return(error_rate)
}

# some predictor combinations
predictor_combinations = list( Lag2      = c("Lag2"), 
                              Lag1_2    = c("Lag1", "Lag2"),
                              Lag2_3    = c("Lag2", "Lag3"), 
                              Lag2_4    = c("Lag2", "Lag4"),
                              Lag2_5    = c("Lag2", "Lag5"),
                              Lag2_V    = c("Lag2", "Volume"),
                              Lag1_2_3  = c("Lag1", "Lag2", "Lag3"),
                              Lag1_2_4  = c("Lag1", "Lag2", "Lag4"), 
                              Lag1_2_V  = c("Lag1", "Lag2", "Volume") )

# logistic regression
err_logis = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("logistic", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test)
  err_logis = c(err_logis, c(err))
}
names(err_logis) = names(predictor_combinations)

# lda
err_lda = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("lda", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test)
  err_lda = c(err_lda, c(err))
}
names(err_lda) = names(predictor_combinations)

# qda
err_qda = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("qda", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test)
  err_qda = c(err_qda, c(err))
}
names(err_qda) = names(predictor_combinations)

# knn_1
set.seed(1)
err_knn1 = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("knn", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test, k=1)
  err_knn1 = c(err_knn1, c(err))
}
names(err_knn1) = names(predictor_combinations)

# knn_2
set.seed(1)
err_knn2 = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("knn", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test, k=2)
  err_knn2 = c(err_knn2, c(err))
}
names(err_knn2) = names(predictor_combinations)

# knn_3
set.seed(1)
err_knn3 = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("knn", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test, k=3)
  err_knn3 = c(err_knn3, c(err))
}
names(err_knn3) = names(predictor_combinations)

# knn_4
set.seed(1)
err_knn4 = c()
for (pred_comb in names(predictor_combinations)){
  err = do_classify("knn", Weekly, predictor_combinations[[pred_comb]], "Direction", train, test, k=4)
  err_knn4 = c(err_knn4, c(err))
}
names(err_knn4) = names(predictor_combinations)


error_rates = data.frame(logistic=err_logis, lda=err_lda, qda=err_qda, knn1=err_knn1, knn2=err_knn2, knn3=err_knn3, knn4=err_knn4)
error_rates
colMeans(error_rates)
rowMeans(error_rates)
# It looks like the overall error rate of 37.5 % obtained by logistic regression and lda 
# when using only Lag2 is the smallest error rate we can achieve.


# 11
library(ISLR)

# 11a

mpg01 = rep(0, nrow(Auto))
mpg_median = median(Auto$mpg)
mpg01[Auto$mpg > mpg_median] = 1
mpg01 = factor(mpg01)
Auto01 = data.frame(Auto[,-1], mpg01)
Auto01$origin = factor(Auto01$origin)

# 11b
# scatter plots
plot(Auto01)

# box plot of predictors vs mpg01
par(mfrow=c(2,4))
for (n in names(Auto01)){
  if (n != "mpg01"){
    plot(Auto01[, "mpg01"], Auto01[, n], xlab="mpg01", ylab=n)
  }
}

# The following features seem to be usefull in predicting mpg01:
# cylinders, displacement, horsepower, weight. The larger value of these
# predictors are associated with mpg01 being below median.
# acceleration and year are also associated with mpg01: but no as strongly.


# 11c
set.seed(1)
ntrains = floor( nrow(Auto01) * (2/3) )
ntests = nrow(Auto01) - ntrains
train = c(rep(TRUE, ntrains), rep(FALSE, ntests))
# to shuffle 
train = sample(train, replace=FALSE)
test = !train
mpg01_test = Auto01[test, "mpg01"]
sum(mpg01_test == 1) / length(mpg01_test)
# about 50% are high mpg

# 11d
library(MASS)
lda_fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year, data=Auto01, subset=train)
lda_pred = predict(lda_fit, Auto01[test, ])$class
confusion = table(mpg01_test, lda_pred)
confusion
1 - sum(diag(confusion)) / sum(confusion) 
# test error rate is 12.2 %


# 11e
qda_fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year, data=Auto01, subset=train)
qda_pred = predict(qda_fit, Auto01[test, ])$class
confusion = table(mpg01_test, qda_pred)
confusion
# error rate
1 - sum(diag(confusion)) / sum(confusion) 
# test error rate is 11.4 % a liite better than lda


# 11f
logistic_fit = glm(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration + year, 
                   data=Auto01, family = binomial, subset = train)
summary(logistic_fit)
# those predictors are statistically significant:
# weight, acceleration, year

logistic_probs = predict(logistic_fit, Auto01[test, ], type="response")
logistic_pred = rep(0, length(logistic_probs)) 
logistic_pred[logistic_probs > 0.5] = 1
logistic_pred = factor(logistic_pred)
confusion = table(mpg01_test, logistic_pred)
confusion
# error rate
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 16.8 %, a liitle worse than both lda and qda


# use only weight, acceleration, and year
logistic_fit = glm(mpg01 ~ weight + acceleration + year, 
                   data=Auto01, family = binomial, subset = train)
summary(logistic_fit)

logistic_probs = predict(logistic_fit, Auto01[test, ], type="response")
logistic_pred = rep(0, length(logistic_probs)) 
logistic_pred[logistic_probs > 0.5] = 1
logistic_pred = factor(logistic_pred)
confusion = table(mpg01_test, logistic_pred)
confusion
# error rate
1 - sum(diag(confusion)) / sum(confusion)
# test error rate 14.5 %, better than using all predictors


# let's try again lda, qda using weight, acceleration, and year
lda_fit = lda(mpg01 ~  weight + acceleration + year, data=Auto01, subset=train)
lda_pred = predict(lda_fit, Auto01[test, ])$class
confusion = table(mpg01_test, lda_pred)
confusion
1 - sum(diag(confusion)) / sum(confusion) 
# test error rate is 14.5 % 

qda_fit = qda(mpg01 ~ weight + acceleration + year, data=Auto01, subset=train)
qda_pred = predict(qda_fit, Auto01[test, ])$class
confusion = table(mpg01_test, qda_pred)
confusion
# error rate
1 - sum(diag(confusion)) / sum(confusion) 
# test error rate is 12.2 % 


# 11g
library(class)
train.X = scale(Auto01[train, c("cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])
train.Y = Auto01[train, ]$mpg01
test.X = scale(Auto01[test, c("cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])
test.Y = Auto01[test, ]$mpg01

# k=1
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=1)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 12.9 %

# k=2
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=2)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 11.4 %

# k=3
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=3)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 11.4 %

# k=4
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=4)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 11.4 %

# k=5
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=5)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 11.4 %

# k=6
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=6)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 12.2 %

# k=7
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=7)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 12.9 %

# k=8
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=8)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 12.2 %

# k=9
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=9)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 12.2 %

# k=10
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=10)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 11.4 %

# k=20
set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=20)
confusion = table(mpg01_test, knn_pred)
1 - sum(diag(confusion)) / sum(confusion)
# test error rate is 11.4 %


# 12a
Power = function(){print(2^3)}
Power

# 12b
Power2 = function(x, a){print(x^a)}
Power2(3, 8)

# 12c
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# 12d
Power3 = function(x, a){return(x^a)}

# 12e
xs = seq.int(1, 10)
ys = Power3(xs, 2)
# type="b" means use points and line
plot(xs, ys, type="b", xlab="x", ylab="y = x^2", main="graph of y = x^2")
# log scale
plot(xs, ys, type="b", xlab="x", ylab="y = x^2", main="graph of y = x^2", log="x")
plot(xs, ys, type="b", xlab="x", ylab="y = x^2", main="graph of y = x^2", log="y")
plot(xs, ys, type="b", xlab="x", ylab="y = x^2", main="graph of y = x^2", log="xy")

# 12f
PlotPower = function(xs, a){
  ys = Power3(xs, a)
  ylabel = sprintf("y = x^%d", a)
  title = sprintf("graph of y = x^%d", a)
  plot(xs, ys, type="b", xlab="x", ylab=ylabel, main=title)
}



# 13
library(MASS)
library(class)

# modify a little bit the function I defined in 10i

do_classify = function(method, data, predictors, 
                       response, train, test,
                       k=NULL){
  data$Y = data[, response]
  variable_names = c(predictors, "Y")
  
  if (method == "logistic"){
    model_fit = glm(Y ~ ., data=data[, variable_names], family=binomial, subset=train)
    
    probs = predict(model_fit, data[test, variable_names], type="response")
    pred = rep("Down", length(probs))
    pred[probs > 0.5] = "Up"
  }
  
  if (method == "lda"){
    model_fit = lda(Y ~ ., data=data[, variable_names], subset=train)
    pred = predict(model_fit, data[test, variable_names])$class
  }
  
  if (method == "qda"){
    model_fit = qda(Y ~ ., data=data[, variable_names], subset=train)
    pred = predict(model_fit, data[test, variable_names])$class
  }
  
  if (method == "knn"){
    only_numeric = c()
    for (name in predictors){
      if  ( class(data[, name]) == "numeric" ){
        only_numeric = c(only_numeric, name)
    }
    
    train.X = scale( data[train, only_numeric] )
    train.Y = data[train,]$Y
    test.X = scale( data[test, only_numeric] )
    pred = knn(train.X, test.X, train.Y, k=k)
    }
  }
  
  confus = table(data$Y[test], pred)
  error_rate = 1 - sum(diag(confus)) / sum(confus)
  
  list_to_return = list(error_rate=error_rate, confusion_matrix=confus)
  if (method != "knn"){
    list_to_return$model_fit = model_fit
  }
    
  return(list_to_return)
}

cor(Boston)
# All except chas (which is factor) are more or less associated with crim
Boston$chas = factor(Boston$chas)
pairs(Boston)

# box plot of predictors vs crim
par(mfrow=c(3,5))
for (n in names(Boston)){
  if (n != "crim"){
    plot(Boston[, n], Boston[, "crim"], xlab=n, ylab="crim")
  }
}


crim_median = median(Boston$crim)
crim01 = rep(0, nrow(Boston))
crim01[Boston$crim > crim_median] = 1
crim01 = factor(crim01)
Boston01 = data.frame(Boston[, -1], crim01)


# prepare train and test data
ntrains = floor(nrow(Boston01) * (1/2))
ntests = nrow(Boston01) - ntrains

set.seed(1)
train = c(rep(TRUE, ntrains), rep(FALSE, ntests))
# to shuffle 
train = sample(train, replace=FALSE)
test = !train
crim01_test = Boston01[test, "crim01"]
sum(crim01_test == 1) / length(crim01_test)
# about 52% are above median


all_variables = names(Boston01)
response = "crim01"
all_predictors = all_variables[ ! all_variables %in% c(response)]

# logistic regression for all predictors
logistic_result = do_classify("logistic", Boston01, all_predictors, 
                       response, train, test)

logistic_result$error_rate    # 6.8 %
summary(logistic_result$model_fit)

# let's choose predictor combinations according to number of stars

predictor_combinations = list(
  two_predictors = c("nox", "rad"),
  three_predictors = c("nox", "rad", "ptratio"),
  seven_predictors = c("nox", "rad", "ptratio", "zn", "dis", "tax", "medv"),
  eight_predictors = c("nox", "rad", "ptratio", "zn", "dis", "tax", "medv", "black"),
  all_predictors = all_predictors
)

# logistic regression
err_logis = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("logistic", Boston01, predictor_combinations[[pred_comb]], response, train, test)
  err_logis = c(err_logis, c(results$error_rate))
}
names(err_logis) = names(predictor_combinations)


# lda
err_lda = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("lda", Boston01, predictor_combinations[[pred_comb]], response, train, test)
  err_lda = c(err_lda, c(results$error_rate))
}
names(err_lda) = names(predictor_combinations)

# qda
err_qda = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("qda", Boston01, predictor_combinations[[pred_comb]], response, train, test)
  err_qda = c(err_qda, c(results$error_rate))
}
names(err_qda) = names(predictor_combinations)


# knn 1
set.seed(1)
err_knn1 = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("knn", Boston01, predictor_combinations[[pred_comb]], response, train, test, k=1)
  err_knn1 = c(err_knn1, c(results$error_rate))
}
names(err_knn1) = names(predictor_combinations)

# knn 2
set.seed(1)
err_knn2 = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("knn", Boston01, predictor_combinations[[pred_comb]], response, train, test, k=2)
  err_knn2 = c(err_knn2, c(results$error_rate))
}
names(err_knn2) = names(predictor_combinations)

# knn 4
set.seed(1)
err_knn4 = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("knn", Boston01, predictor_combinations[[pred_comb]], response, train, test, k=4)
  err_knn4 = c(err_knn4, c(results$error_rate))
}
names(err_knn4) = names(predictor_combinations)

# knn 6
set.seed(1)
err_knn6 = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("knn", Boston01, predictor_combinations[[pred_comb]], response, train, test, k=6)
  err_knn6 = c(err_knn6, c(results$error_rate))
}
names(err_knn6) = names(predictor_combinations)

# knn 10
set.seed(1)
err_knn10 = c()
for (pred_comb in names(predictor_combinations)){
  results = do_classify("knn", Boston01, predictor_combinations[[pred_comb]], response, train, test, k=10)
  err_knn10 = c(err_knn10, c(results$error_rate))
}
names(err_knn10) = names(predictor_combinations)

error_rates = data.frame(logistic=err_logis, lda=err_lda, qda=err_qda, 
                         knn1=err_knn1, knn2=err_knn2, knn4=err_knn4, knn6=err_knn6, knn10=err_knn10)

colMeans(error_rates)


