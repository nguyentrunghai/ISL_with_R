
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

