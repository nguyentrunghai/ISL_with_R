
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

