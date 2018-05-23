
# 6.8 Exercises

# Conceptual

# 1a
# In general, best subset selection will give the smallest training RSS.
# For a given number of predictors k, best subset selection method performs exhaustive search over all
# possible combinations of predictors for the one with lowest RSS. 
# Therefore, it guarantees to find the global minimum.
# For forward and backward stepwise selection methods, the search is not exhaustive.
# At each k, it can only add one or remove one predictor from the optimal set found in previous step.
# There is no guarantee that they will find the global minimum.

# 1b 
# Can't tell for sure, although if k is not too large such that overfitting is not likely to happen, 
# it is expected that model given by best subset selection may give the lowest test RSS.
# This is becuase the predictor subset chosen by the best subset selection may be associated better with the response.

# 1c
# i   TRUE
# ii  TRUE
# iii FALSE, the two methods may follow different search paths
# iv  FALSE
# v   FALSE, because it performs exhaustive seach for each k, there is no restriction on
#            which predictors to include for each k.



#---------------------
# 2a
# i   INCORRECT: Lasso is less flexible than least squares.
# ii  INCORRECT: reason as above
# iii CORRECT: In lasso all predictors are shrunk toward zero and some of them may be exactly zero.
#              So the model given by lasso is less flexible than least squares.
# iv  INCORRECT: less flexible means more bias and less variance, not the other way around.

# 2b
# Ridge regression is similar to lasso in terms of shrinking all predictors toward zero, 
# and hence producing models that are less flexible 
# than least squares. So the answers are the same as in 2a

# 2c
# Non-linear models are more flexible linear least squares models.
# And more flexible means less bias and more variance.
# i   INCORRECT
# ii  CORRECT
# iii INCORRECT
# iv  INCORRECT


#----------------------
# 3a
# When s = 0, we have the null model with only the intercep. 
# When s increases from 0, the model becomes more and more flexible and fits better and better the training set.
# iv is correct

# 3b
# Due to bias-variance tradeoff, the test RSS (or test MSE) will have a characteristic u shape.
# ii is correct

# 3c
# Variance always increases as model flexibility increases.
# iii is correct

# 3d
# Squared bias always decreases as model flexibility increases.
# iv is correct

# 3e
# Irreducible error cannot be predicted by predictors so remains constant.
# v is correct


#----------------------------
# 4a
# As lambda increases from 0, the model becomes less and less flexible 
# and therefore, the training RSS will increases.
# iii is correct

# 4b
# ii is correct

# 4c
# Variance always decreases when the model becomes less and less flexible
# iv is correct

# 4d
# Squared bias always increases with decreasing flexibility
# iii is correct

# 4e
# v is correct




# Applied

# 8a
set.seed(1)
x = rnorm(100)
e = rnorm(100)


# 8b
y = 5 + 2*x + 4*x^2 + x^3 + e


# 8c
library(leaps)
data = data.frame(x=x, y=y)

model_sel = list()
coeffi = list()

regfit_best = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10),
                            data=data, nvmax=10)

# this fit using poly(x, 10) gives very different coefficients, but the same prediction?
# regfit_bestsub = regsubsets(y ~ poly(x, 10), data=data, nvmax=10)


(summary_best = summary(regfit_best))
names(summary_best)

# plot Cp, BIC and adjusted R2
par(mfrow=c(3,3))

# Cp
plot(summary_best$cp, xlab="Number of predictors", ylab="Cp", type="l")
(cp_min = which.min(summary_best$cp))  # 4
model_sel[["best_cp"]] = cp_min
points(cp_min, summary_best$cp[cp_min], col="red", cex=2, pch=20)
coeffi[["best_cp"]] = coefficients(regfit_best, cp_min)

# BIC
plot(summary_best$bic, xlab="Number of predictors", ylab="BIC", type="l")
(bic_min = which.min(summary_best$bic))  # 3
model_sel[["best_bic"]] = bic_min
points(bic_min, summary_best$bic[bic_min], col="red", cex=2, pch=20)
coeffi[["best_bic"]] = coefficients(regfit_best, bic_min)

# Adjusted R2
plot(summary_best$adjr2, xlab="Number of predictors", ylab="Adj. R2", type="l")
(adjr2_max = which.max(summary_best$adjr2))  # 4
model_sel[["best_adjr2"]] = adjr2_max
points(adjr2_max, summary_best$adjr2[adjr2_max], col="red", cex=2, pch=20)
coeffi[["best_adjr2"]] = coefficients(regfit_best, adjr2_max)

# Of course we know that the true model is degree-3 polynomial, and BIC gives correct answer.
# Although Cp chose a model with 4 predictors and adjusted R2 chose the one with 5 predictors,
# their plots show no significant improvement after three predictors have been included.


# 8c
# foward stepwise selection
set.seed(1)
regfit_fw = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10),
                            data=data, nvmax=10, method="forward")

(summary_fw = summary(regfit_fw))

# Cp
plot(summary_fw$cp, xlab="Number of predictors", ylab="Cp", type="l")
(cp_min = which.min(summary_fw$cp))  # 4
model_sel[["fw_cp"]] = cp_min
points(cp_min, summary_fw$cp[cp_min], col="red", cex=2, pch=20)
coeffi[["fw_cp"]] = coefficients(regfit_fw, cp_min)

# BIC
plot(summary_fw$bic, xlab="Number of predictors", ylab="BIC", type="l")
(bic_min = which.min(summary_fw$bic))  # 3
model_sel[["fw_bic"]] = bic_min
points(bic_min, summary_fw$bic[bic_min], col="red", cex=2, pch=20)
coeffi[["fw_bic"]] = coefficients(regfit_fw, bic_min)

# Adjusted R2
plot(summary_fw$adjr2, xlab="Number of predictors", ylab="Adj. R2", type="l")
(adjr2_max = which.max(summary_fw$adjr2))  # 4
model_sel[["fw_adjr2"]] = adjr2_max
points(adjr2_max, summary_fw$adjr2[adjr2_max], col="red", cex=2, pch=20)
coeffi[["fw_adjr2"]] = coefficients(regfit_fw, adjr2_max)


# foward stepwise selection
set.seed(1)
regfit_bw = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10),
                       data=data, nvmax=10, method="backward")

(summary_bw = summary(regfit_bw))

# Cp
plot(summary_bw$cp, xlab="Number of predictors", ylab="Cp", type="l")
(cp_min = which.min(summary_bw$cp))  # 4
model_sel[["bw_cp"]] = cp_min
points(cp_min, summary_bw$cp[cp_min], col="red", cex=2, pch=20)
coeffi[["bw_cp"]] = coefficients(regfit_bw, cp_min)

# BIC
plot(summary_bw$bic, xlab="Number of predictors", ylab="BIC", type="l")
(bic_min = which.min(summary_bw$bic))  # 3
model_sel[["bw_bic"]] = bic_min
points(bic_min, summary_bw$bic[bic_min], col="red", cex=2, pch=20)
coeffi[["bw_bic"]] = coefficients(regfit_bw, bic_min)

# Adjusted R2
plot(summary_bw$adjr2, xlab="Number of predictors", ylab="Adj. R2", type="l")
(adjr2_max = which.max(summary_bw$adjr2))  # 4
model_sel[["bw_adjr2"]] = adjr2_max
points(adjr2_max, summary_bw$adjr2[adjr2_max], col="red", cex=2, pch=20)
coeffi[["bw_adjr2"]] = coefficients(regfit_bw, adjr2_max)

coeffi
# Best subset and forward step wise selection methods give the same answer.
# This kind of makes sense becuase the true model happens to include x, x^2, x^3
# which are in the search order for forward stepwise selection.
# Backward stepwise selection gives different results.

# look at the best model in scatter plot
par(mfrow=c(1,1))
test_data = data.frame(x=seq(min(x), max(x), length.out=10), y=numeric(10))
test_x_matrix = model.matrix(as.formula(regfit_best$call[[2]]), test_data)
coefi = coefficients(regfit_best, id=model_sel[["best_bic"]])
pred_bestsub = test_x_matrix[, names(coefi)] %*% coefi
plot(x,y)
lines(test_data$x, pred_bestsub, col="red")

