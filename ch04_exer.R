
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
summary(logistic_fit)$coef
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
train.X = cbind( Weekly[train,]$Lag2 )
train.Y = Weekly[train,]$Direction

test.X = cbind( Weekly[test,]$Lag2 )

set.seed(1)
knn_pred = knn(train.X, test.X, train.Y, k=1)
confusion = table(Direction_true_after2008, knn_pred)

# overall fraction of correct prediction
sum(diag(confusion)) / sum(confusion)
# 50. %, the error rate is 50. %, like random guessing!

# 10h
# test error rates
# logistic regression: 37.5 %
# LDA:                 37.5 %
# QDA:                 41.4 %
# KNN with k=1:        50.0 %
# So logistic regression and lda apprea to perform the best

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
    train.X = cbind( data[train, predictors] )
    train.Y = data[train,]$Y
    test.X = cbind( data[test, predictors] )
    
    set.seed(1)
    pred = knn(train.X, test.X, train.Y, k=k)
  }
  
  confus = table(data$Y[test], pred)
  error_rate = 1 - sum(diag(confus)) / sum(confus)
  return(error_rate)
}

# some predictor combinations
preditor_combinations = list( Lag2      = c("Lag2"), 
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
for (pred_comb in names(preditor_combinations)){
  err = do_classify("logistic", Weekly, preditor_combinations[[pred_comb]], "Direction", train, test)
  err_logis = c(err_logis, c(err))
}
names(err_logis) = names(preditor_combinations)

# lda
err_lda = c()
for (pred_comb in names(preditor_combinations)){
  err = do_classify("lda", Weekly, preditor_combinations[[pred_comb]], "Direction", train, test)
  err_lda = c(err_lda, c(err))
}
names(err_lda) = names(preditor_combinations)

# qda
err_qda = c()
for (pred_comb in names(preditor_combinations)){
  err = do_classify("qda", Weekly, preditor_combinations[[pred_comb]], "Direction", train, test)
  err_qda = c(err_qda, c(err))
}
names(err_qda) = names(preditor_combinations)

# knn_1
err_knn1 = c()
for (pred_comb in names(preditor_combinations)){
  err = do_classify("knn", Weekly, preditor_combinations[[pred_comb]], "Direction", train, test, k=1)
  err_knn1 = c(err_knn1, c(err))
}
names(err_knn1) = names(preditor_combinations)

# knn_2
err_knn2 = c()
for (pred_comb in names(preditor_combinations)){
  err = do_classify("knn", Weekly, preditor_combinations[[pred_comb]], "Direction", train, test, k=2)
  err_knn2 = c(err_knn2, c(err))
}
names(err_knn2) = names(preditor_combinations)

# knn_3
err_knn3 = c()
for (pred_comb in names(preditor_combinations)){
  err = do_classify("knn", Weekly, preditor_combinations[[pred_comb]], "Direction", train, test, k=3)
  err_knn3 = c(err_knn3, c(err))
}
names(err_knn3) = names(preditor_combinations)

error_rates = data.frame(logistic=err_logis, lda=err_lda, qda=err_qda, knn1=err_knn1, knn2=err_knn2, knn3=err_knn3)
error_rates
# It looks like the overall error rate of 37.5 % obtained by logistic regression and lda 
# when using only Lag2 is the smallest error rate we can achieve.


