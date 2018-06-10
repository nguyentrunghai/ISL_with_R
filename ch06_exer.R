
# 6.8 Exercises

# Conceptual

# 1a
# In general, best subset selection will give the smallest training RSS.
# For a given number of predictors k, best subset selection method performs exhaustive search over all
# possible combinations of predictors for the one with lowest RSS. 
# Therefore, it guarantees to find the global minimum.
# For forward and backward stepwise selection methods, the search is not exhaustive.
# At each k, it can only add one or remove one predictor from the optimal set found in the previous step.
# There is no guarantee that forward and backward stepwise selection methods will find the global minimum.

# 1b 
# Can't tell for sure, each k-variable model was selected based on training RSS or R^2 which 
# do not tell much about test error.

# 1c
# i   TRUE
# ii  TRUE
# iii FALSE, the two methods may follow different search paths
# iv  FALSE
# v   FALSE, because it performs exhaustive seach for each k, there is no restriction on
#            which predictors to include for each k, ie., each step is independent.



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
# ii is correct, upward U

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

regfit_best = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + 
                           I(x^8) + I(x^9) + I(x^10), data=data, nvmax=10, method="exhaustive")

# this fit using poly(x, 10) gives very different coefficients, but the same prediction?
# regfit_bestsub = regsubsets(y ~ poly(x, 10), data=data, nvmax=10, method="exhaustive")


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
# Cp and adjusted R2 chose a model with 4 predictors which include also X^5.
# But the coeefficient for X^5 is rather small.


# 8d - forward stepwise selection
set.seed(1)
regfit_fw = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + 
                         I(x^9) + I(x^10), data=data, nvmax=10, method="forward")

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


# 8d - backard stepwise selection
set.seed(1)
regfit_bw = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + 
                         I(x^8) + I(x^9) + I(x^10), data=data, nvmax=10, method="backward")

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
# Best subset and forward step wise selection methods give the same answers and include all x, x^2, x^3.
# This makes sense becuase the true model happens to include x, x^2, x^3
# which are in the search order for forward stepwise selection.
# Backward stepwise selection gives differnt models than best subset and forward step-wise.
# In particular they do not include X^3.

# look at the best model in scatter plot
par(mfrow=c(1,1))
test_data = data.frame(x=seq(min(x), max(x), length.out=10), y=numeric(10))
test_x_matrix = model.matrix(as.formula(regfit_best$call[[2]]), test_data)
coefi = coefficients(regfit_best, id=model_sel[["best_bic"]])
pred_bestsub = test_x_matrix[, names(coefi)] %*% coefi
plot(x,y)
lines(test_data$x, pred_bestsub, col="red")


# 8e
x_matrix = model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                          I(x^7) + I(x^8) + I(x^9) + I(x^10))[,-1]
library(glmnet)
grid = 10^seq(10, -2, length=100)
fit_lasso = glmnet(x_matrix, y, alpha=1, lambda=grid)
dim(coef(fit_lasso))

set.seed(1)
cv_lasso = cv.glmnet(x_matrix, y, alpha=1, nfolds=10)
plot(cv_lasso)
best_lam_lasso = cv_lasso$lambda.min
best_lam_lasso    # 0.03424472
coef_lasso = predict(fit_lasso, type="coefficients", s=best_lam_lasso)
coef_lasso = coef_lasso[,1]
coef_lasso[coef_lasso != 0]
# CV select 6-variable model, x, x^2, x^3 are included.

# plot lasso line in scatter plot
test_data = data.frame(x=seq(min(x), max(x), length.out=10), y=numeric(10))
test_x_matrix = model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                               I(x^7) + I(x^8) + I(x^9) + I(x^10), test_data)[,-1]
pred_lasso = predict(fit_lasso, s=best_lam_lasso, newx=test_x_matrix)
par(mfrow=c(1,1))
plot(x,y)
lines(test_data$x, pred_lasso, col="red")


# 8f 
# best subset 
y = 5 + 2*x^7 + e
data = data.frame(x=x, y=y)

regfit_best = regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + 
                           I(x^8) + I(x^9) + I(x^10), data=data, nvmax=10, method="exhaustive")

(summary_best = summary(regfit_best))

par(mfrow=c(2,3))

# Cp
plot(summary_best$cp, xlab="Number of predictors", ylab="Cp", type="l")
(cp_min = which.min(summary_best$cp))  # 2
points(cp_min, summary_best$cp[cp_min], col="red", cex=2, pch=20)
coefficients(regfit_best, cp_min)  # X^2 and X^7

# BIC
plot(summary_best$bic, xlab="Number of predictors", ylab="BIC", type="l")
(bic_min = which.min(summary_best$bic))  # 1
points(bic_min, summary_best$bic[bic_min], col="red", cex=2, pch=20)
coefficients(regfit_best, bic_min) # X^7

# Adjusted R2
plot(summary_best$adjr2, xlab="Number of predictors", ylab="Adj. R2", type="l")
(adjr2_max = which.max(summary_best$adjr2))  # 4
points(adjr2_max, summary_best$adjr2[adjr2_max], col="red", cex=2, pch=20)
coefficients(regfit_best, adjr2_max)   # X, X^2, X^3, X^7


# lasso
x_matrix = model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                          I(x^7) + I(x^8) + I(x^9) + I(x^10))
grid = 10^seq(10, -2, length=100)
fit_lasso = glmnet(x_matrix, y, alpha=1, lambda=grid, standardize=TRUE)

set.seed(1)
cv_lasso = cv.glmnet(x_matrix, y, alpha=1, nfolds=10)
plot(cv_lasso)
best_lam_lasso = cv_lasso$lambda.min
best_lam_lasso    # 3.879577
coef_lasso = predict(fit_lasso, type="coefficients", s=best_lam_lasso)
coef_lasso = coef_lasso[,1]
coef_lasso[coef_lasso != 0]   # X^7

# Best subset selection with lowest BIC gives the model as 4.95894 + 2.00077*X^7
# Lasso with 10-fold CV give the best model as 5.229085 + 1.936760 * X^7
# The two models are similar and very close the the true model. 
# The beta_7 coefficients of the lasso is smaller than that of best subset, which may be due to
# the coefficient shrinkage of the lasso


# 9
library(ISLR)
library(glmnet)
names(College)  # in ISLR

# 9a
set.seed(1)
train = sample(1:nrow(College), size=nrow(College)/2, replace=FALSE)
test = -train

# 9b
fit_ls = lm(Apps ~ ., data=College[train,])
summary(fit_ls)
pred_ls = predict(fit_ls, newdata=College[test,])
(test_mse_ls = mean((pred_ls - College$Apps[test])^2))   # 1108531

# 9c
train_x_matrix = model.matrix(Apps ~ ., data=College[train,])[,-1]
grid = 10^seq(10, -2, length=100)
fit_ridge = glmnet(train_x_matrix, College$Apps[train], alpha=0, lambda=grid) 

set.seed(1)
cv_ridge = cv.glmnet(train_x_matrix, College$Apps[train], alpha=0, nfolds=10)
plot(cv_ridge)
(best_lam_ridge = cv_ridge$lambda.min) # 450.7435
(coef_ridge = predict(fit_ridge, type="coefficients", s=best_lam_ridge))

test_x_matrix = model.matrix(Apps ~ ., data=College[test,])[,-1]
# consistency check
# test if it reproduce least squares (lambda = 0)
pred_ridge = predict(fit_ridge, s=0, newx=test_x_matrix)
mean((pred_ridge - College$Apps[test])^2)  # 1108447
# with very big lambda so that the model predict mean y for every test x
pred_ridge = predict(fit_ridge, s=1e20, newx=test_x_matrix)
mean((pred_ridge - College$Apps[test])^2)    # 11263629
# ls model with only the intercept
mean( ( mean(College$Apps[train]) - College$Apps[test])^2 ) # 11263668

pred_ridge = predict(fit_ridge, s=best_lam_ridge, newx=test_x_matrix)
(test_mse_ridge = mean((pred_ridge - College$Apps[test])^2))  # 1036914
# very close to the test error given by least squares.
# The results seem to change a lot with the seed. Much more so for the ridge regression.
# This may be due to variance in 10-fold cv estimate of test error, which is used to select the best model.

# 9d
fit_lasso = glmnet(train_x_matrix, College$Apps[train], alpha=1, lambda=grid) 
set.seed(1)
cv_lasso = cv.glmnet(train_x_matrix, College$Apps[train], alpha=1, nfolds=10)
plot(cv_lasso)
(best_lam_lasso = cv_lasso$lambda.min) # 24.62086
coef_lasso = predict(fit_lasso, type="coefficients", s=best_lam_lasso)[,1]
length(coef_lasso) # 18
length(coef_lasso[coef_lasso!=0]) # 16
# Only two predictors is shrunk to zero.

pred_lasso = predict(fit_lasso, s=best_lam_lasso, newx=test_x_matrix)
(test_mse_lasso = mean((pred_lasso - College$Apps[test])^2))  # 1032128
# almost the same as ridge regression

# 9e
library(pls)
set.seed(1)
fit_pcr = pcr(Apps ~ ., data=College[train, ], scale=TRUE, validation="CV")
summary(fit_pcr)
validationplot(fit_pcr, val.type="MSEP", type="b")
# lowest MSE is when ncomp = 16, almost no dimensional reduction.
pred_pcr = predict(fit_pcr, newdata=College[test, ], ncomp=16)
(test_mse_pcr = mean((pred_pcr - College$Apps[test])^2))  # 1166897
# very close to ridge and lasso regression

# 9f
set.seed(1)
fit_pls = plsr(Apps ~ ., data=College[train, ], scale=TRUE, validation="CV")
summary(fit_pls)
validationplot(fit_pls, val.type="MSEP", type="b")
# lowest MSE is when ncomp = 10
pred_pls = predict(fit_pls, newdata=College[test, ], ncomp=10)
(test_mse_pls = mean((pred_pls - College$Apps[test])^2))  # 1134531
# almost the same as pcr

# 9g
test_mse_ls      # 1108531
test_mse_ridge   # 1036914
test_mse_lasso   # 1032128
test_mse_pcr     # 1166897
test_mse_pls     # 1134531


