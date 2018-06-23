

# Applied

# 6a  look back at page 192 for how to do cross-validation
library(ISLR)
attach(Wage)
polyno_reg_models = list()
max_degree = 10

for(deg in 1:max_degree)
{
  polyno_reg_models[[deg]] = glm(wage ~ poly(age, degree=deg, raw=TRUE), data=Wage)
}

# 10-fold cross validation
library(boot)
set.seed(1)
cv_fold = 10
cv.errors = rep(0, max_degree)
for(deg in 1:max_degree)
{
  cv.errors[deg] = cv.glm(Wage, polyno_reg_models[[deg]], K=cv_fold)$delta[1]
}
plot(cv.errors, type="b")
cv_min = which.min(cv.errors)  # 4
points(cv_min, cv.errors[cv_min], col="red")
# based on 10-fold cross validation the best model has up to the 4th degree

# ANOVA
polyno_reg_models = list()
for(deg in 1:max_degree)
{
  polyno_reg_models[[deg]] = lm(wage ~ poly(age, degree=deg, raw=TRUE), data=Wage)
}
do.call(anova, c(as.vector(polyno_reg_models), test="F") )

# there is little evidence that the 5th is better than the 4th based on the p-value of F-statistic
# so we should choose model with up to the 4th degree polynomial

best_fit = lm(wage ~ poly(age, degree=cv_min, raw=TRUE), data=Wage)
agelims = range(age)
age.grid = seq(from = agelims[1], to=agelims[2])
preds = predict(best_fit, newdata = data.frame(age = age.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(age, wage)
title( paste( c("Degree -", cv_min, "Polynomial"), collapse = " "))
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty="dashed")


# 6b
# this gives error
set.seed(1)
cv_fold = 10
max_cuts = 10
cv.errors = rep(NA, max_cuts)
for(i in 2:max_cuts)
{
  Wage$age_c = cut(age, i)
  step_reg_model = glm(wage ~ age_c, data=Wage)
  cv.errors[i] = cv.glm(Wage, step_reg_model, K=cv_fold)$delta[1]
}

plot(cv.errors, type="b")
min_error_cut = which.min(cv.errors)
points(min_error_cut, cv.errors[min_error_cut], col="red")

# fit using min_error_cut
Wage$age_c = cut(age, min_error_cut)
step_reg_model = glm(wage ~ age_c, data=Wage)
preds = predict(step_reg_model, newdata=data.frame(age_c = cut(age.grid, min_error_cut) ), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(age, wage)
title( paste( c("Number of cuts -", min_error_cut), collapse = " "))
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty="dashed")


# 7 
# lets include the variables in this order s(age, 5), year, education, maritl, jobclass
# and do hypothesis testing with ANOVA
library(gam)

gam.fit1 = gam(wage ~ s(age, 5), data=Wage)
gam.fit2 = gam(wage ~ s(age, 5) + year, data=Wage)
gam.fit3 = gam(wage ~ s(age, 5) + year + education, data=Wage)
gam.fit4 = gam(wage ~ s(age, 5) + year + education + maritl, data=Wage)
gam.fit5 = gam(wage ~ s(age, 5) + year + education + maritl + jobclass, data=Wage)

anova(gam.fit1, gam.fit2, gam.fit3, gam.fit4, gam.fit5, test="F")
# it looks like there is a strong evidence of needing to use the most complex model, gam.fit5

par(mfrow=c(2, 3))
plot(gam.fit5, se=TRUE, col="blue")
contrasts(jobclass)
# There are clear relationship between predictors and response, except for maritl.
# For maritl there is a clear difference in terms of wage between levels "1. Never Married" and "2. Married"
# The other levels, eg., "3. Widowed", "4. Divorced" and "5. Separated" are noisy 
# which is due to small number of cases
mean(maritl == "3. Widowed")*100    # 0.6 %
mean(maritl == "4. Divorced")*100   # 6.8 %
mean(maritl == "5. Separated")*100  # 1.8 %
# less remove this level and replot the figures
gam.fit5 = gam(wage ~ s(age, 5) + year + education + maritl + jobclass, 
               data=Wage[maritl != "3. Widowed" & maritl != "4. Divorced" & maritl != "5. Separated", ])
par(mfrow=c(2, 3))
plot(gam.fit5, se=TRUE, col="blue")

# Nevetheless, the contribution magnitude of maritl and jobclass is rather small.


# 8
Auto$origin = factor(Auto$origin)
Auto$cylinders = factor(Auto$cylinders)
pairs(Auto)
# From scatter plots, there are some non-linear relation between predictors such as horsepower, weight, 
# and the response mpg

max_df = 10
gam_models = list()

for(df in 1:max_df)
{
  gam_models[[df]] = gam(mpg ~ s(displacement, df), data=Auto)
}
do.call(anova, c(as.vector(gam_models), test="F"))

# There is little evidence that we should use more than 3 df for displacement

for(df in 1:max_df)
{
  gam_models[[df]] = gam(mpg ~ s(horsepower, df), data=Auto)
}
do.call(anova, c(as.vector(gam_models), test="F"))
# There is little evidence that we should use more than 5 df for horsepower

for(df in 1:max_df)
{
  gam_models[[df]] = gam(mpg ~ s(weight, df), data=Auto)
}
do.call(anova, c(as.vector(gam_models), test="F"))
# There is little evidence that we should use more than 2 df for weight

gam.fit1 = gam(mpg ~ cylinders, data=Auto)
gam.fit2 = gam(mpg ~ cylinders + s(displacement, 3), data=Auto)
gam.fit3 = gam(mpg ~ cylinders + s(displacement, 3) + s(horsepower, 5), data=Auto)
gam.fit4 = gam(mpg ~ cylinders + s(displacement, 3) + s(horsepower, 5) + s(weight, 5), data=Auto)
gam.fit5 = gam(mpg ~ cylinders + s(displacement, 3) + s(horsepower, 5) + s(weight, 5) + 
                 acceleration, data=Auto)
gam.fit6 = gam(mpg ~ cylinders + s(displacement, 3) + s(horsepower, 5) + s(weight, 5) + 
                acceleration + year, data=Auto)
gam.fit7 = gam(mpg ~ cylinders + s(displacement, 3) + s(horsepower, 5) + s(weight, 5) + 
                 acceleration + year + origin, data=Auto)

anova(gam.fit1, gam.fit2, gam.fit3, gam.fit4, gam.fit5, gam.fit6, gam.fit7, test="F")
# there is strong evident to use the most complex model gam.fit7
par(mfrow=c(2, 4))
plot(gam.fit7, se=TRUE, col="blue")
# predictors having strong relation with response are horsepower, weight, acceleration and year


# 9a
library(MASS)
plot(Boston$dis, Boston$nox)
cubic_polyn_fit = lm(nox ~ poly(dis, 3, raw=TRUE), data=Boston)
summary(cubic_polyn_fit)

dislims = range(Boston$dis)
dis_grid = seq(from=dislims[1], to=dislims[2], length.out=100)
preds = predict(cubic_polyn_fit, newdata=data.frame(dis=dis_grid), se=TRUE)
se.bands = cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)
lines(dis_grid, preds$fit, lw=2, col="blue")
matlines(dis_grid, se.bands, col="blue", lty="dashed")
title("Cubic polynomial fit")

# 9b
par(mfrow = c(3, 4))
max_degree = 10

for(deg in 1:max_degree)
{
  polyn_fit = lm(nox ~ poly(dis, deg, raw=TRUE), data=Boston)
  rss = sum((polyn_fit$residuals)^2)
  preds = predict(polyn_fit, newdata=data.frame(dis=dis_grid), se=TRUE)
  se.bands = cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)
  
  plot(Boston$dis, Boston$nox)
  lines(dis_grid, preds$fit, lw=2, col="blue")
  matlines(dis_grid, se.bands, col="blue", lty="dashed")
  title( paste( c( "Deg.", deg, ", rss =", format(rss)), collapse =" " ) )
}
# As expected, the more flexible the model is, the smaller the RSS becomes

# 9c
library(boot)
set.seed(1)
max_degree = 10
n_folds = 10
cv.errors = rep(NA, n_folds)

for(deg in 1:max_degree)
{
  polyn_fit = glm(nox ~ poly(dis, deg, raw=TRUE), data=Boston)
  cv.errors[deg] = cv.glm(Boston, polyn_fit, K=n_folds)$delta[1]
}

min_err_deg = which.min(cv.errors)
plot(cv.errors, type="b", xlab="polynomial degree", ylab="CV error")
points(min_err_deg, cv.errors[min_err_deg], col="red")

polyn_fit = glm(nox ~ poly(dis, min_err_deg, raw=TRUE), data=Boston)
preds = predict(polyn_fit, newdata=data.frame(dis=dis_grid), se=TRUE)
se.bands = cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)
plot(Boston$dis, Boston$nox)
lines(dis_grid, preds$fit, lw=2, col="blue")
matlines(dis_grid, se.bands, col="blue", lty="dashed")


# 9d
library(splines)
# by default, bs() gives cubic splines, 3rd degree
attr(bs(Boston$dis, df=4), "degree")
# one knot at the median
attr(bs(Boston$dis, df=4), "knots")

spline_df4_fit = glm(nox ~ bs(dis, df=4), data=Boston)
preds = predict(spline_df4_fit, newdata=data.frame(dis=dis_grid), se=TRUE)
se.bands = cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)
plot(Boston$dis, Boston$nox)
lines(dis_grid, preds$fit, lw=2, col="blue")
matlines(dis_grid, se.bands, col="blue", lty="dashed")

# 9e
par(mfrow=c(3,3))
max_df = 10
for(df in 3:max_df)
{
  spline_fit = glm(nox ~ bs(dis, df=df), data=Boston)
  rss = sum((spline_fit$residuals)^2)
  
  preds = predict(spline_fit, newdata=data.frame(dis=dis_grid), se=TRUE)
  se.bands = cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)
  
  plot(Boston$dis, Boston$nox)
  lines(dis_grid, preds$fit, lw=2, col="blue")
  matlines(dis_grid, se.bands, col="blue", lty="dashed")
  title( paste( c( "D.F.", df, ", rss =", format(rss)), collapse = " " ) )
}
# The more flexible the model is, the smaller the RSS becomes


# 9f
set.seed(1)
max_df = 10
n_folds = 10
cv.errors = rep(NA, max_df)

for(df in 3:max_df)
{
  spline_fit = glm(nox ~ bs(dis, df=df), data=Boston)
  cv.errors[df] = cv.glm(Boston, spline_fit, K=n_folds)$delta[1]
}

plot(cv.errors, type="b")
min_err_df = which.min(cv.errors)
points(min_err_df, cv.errors[min_err_df], col="red")

spline_fit = glm(nox ~ bs(dis, df=min_err_df), data=Boston)
preds = predict(spline_fit, newdata=data.frame(dis=dis_grid), se=TRUE)
se.bands = cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)
plot(Boston$dis, Boston$nox)
lines(dis_grid, preds$fit, lw=2, col="blue")
matlines(dis_grid, se.bands, col="blue", lty="dashed")
title( paste( c("Splines with df", min_err_df), collapse = " ") )


# 10a
library(ISLR)
library(leaps)
names(College)
dim(College)

set.seed(1)
test = sample(1:nrow(College), size=nrow(College)/2, replace=FALSE)
train = - test

fit.fwd = regsubsets(Outstate ~ ., nvmax=ncol(College), data=College[train, ], method="forward")
fit.summ = summary(fit.fwd)
names(fit.summ)

# ploting rss, adjr2, cp and BIC
par(mfrow=c(2,2))
plot(fit.summ$rss, xlab="# variables", ylab="RSS", type="l")

plot(fit.summ$adjr2, xlab="# variables", ylab="adj. R2", type="l")
adjr2_max = which.max(fit.summ$adjr2)
points(adjr2_max, fit.summ$adjr2[adjr2_max], col="red")

plot(fit.summ$cp, xlab="# variables", ylab="CP", type="l")
cp_min = which.min(fit.summ$cp)
points(cp_min, fit.summ$cp[cp_min], col="red")

plot(fit.summ$bic, xlab="# variables", ylab="BIC", type="l")
bic_min = which.min(fit.summ$bic)
points(bic_min, fit.summ$bic[bic_min], col="red")

# bic give the smallest model with 6 variables.
# So we will use this model for latter step

selected_features = names(coefficients(fit.fwd, bic_min))[-1]
selected_features[1] = "Private"


# 10b
library(gam)
pairs( College[, c("Outstate", selected_features)])
# only Expend have somewhat clear non-linear relationship with Outstate 

gam.fit = gam( Outstate ~ Private + Room.Board + PhD + perc.alumni + 
                 poly(Expend, 2, raw=TRUE) + Grad.Rate, data=College[train, ])

par(mfrow=c(2,3))
plot(gam.fit, se=TRUE, col="blue")


# 10c
preds_train = predict(gam.fit)
train_error = mean( (preds_train - College[train, "Outstate"])^2 )

preds_test = predict(gam.fit, newdata=College[test, ])
test_error = mean( (preds_test - College[test, "Outstate"])^2 )
# test_error is even smaller than train_error, underfitting?


# 10d
max_degree = 5

# Room.Board
plot(College$Room.Board, College$Outstate)
gam_models = list()
for(deg in 1:max_degree)
{
  gam_models[[deg]] = gam(Outstate ~ poly(Room.Board, deg, raw=TRUE), data=College[train, ])
}
do.call(anova, c(as.vector(gam_models), test="F") )
# There is no evidence that more conplex function than the linear one is needed.

# PhD
plot(College$PhD, College$Outstate)
gam_models = list()
for(deg in 1:max_degree)
{
  gam_models[[deg]] = gam(Outstate ~ poly(PhD, deg, raw=TRUE), data=College[train, ])
}
do.call(anova, c(as.vector(gam_models), test="F") )
# There is strong evidence that degree 2 polynomial fits better linear

# perc.alumni
plot(College$perc.alumni, College$Outstate)
gam_models = list()
for(deg in 1:max_degree)
{
  gam_models[[deg]] = gam(Outstate ~ poly(perc.alumni, deg, raw=TRUE), data=College[train, ])
}
do.call(anova, c(as.vector(gam_models), test="F") )
# There is no evidence that more complex model than the linear one is needed.

# Expend
plot(College$Expend, College$Outstate)
gam_models = list()
for(deg in 1:max_degree)
{
  gam_models[[deg]] = gam(Outstate ~ poly(Expend, deg, raw=TRUE), data=College[train, ])
}
do.call(anova, c(as.vector(gam_models), test="F") )
# There is strong evidence that degree 3 polynomial fits better degree 2

# Grad.Rate
plot(College$Grad.Rate, College$Outstate)
gam_models = list()
for(deg in 1:max_degree)
{
  gam_models[[deg]] = gam(Outstate ~ poly(Grad.Rate, deg, raw=TRUE), data=College[train, ])
}
do.call(anova, c(as.vector(gam_models), test="F") )
# There is no evidence that more complex model than the linear one is needed.

# let's try to fit a GAM model with the polynomial degrees selected above
gam.fit = gam( Outstate ~ Private + Room.Board + poly(PhD, 2, raw=TRUE) + perc.alumni + 
                 poly(Expend, 3, raw=TRUE) + Grad.Rate, data=College[train, ])

par(mfrow=c(2,3))
plot(gam.fit, se=TRUE, col="blue")

preds_train = predict(gam.fit)
train_error = mean( (preds_train - College[train, "Outstate"])^2 )

preds_test = predict(gam.fit, newdata=College[test, ])
test_error = mean( (preds_test - College[test, "Outstate"])^2 )
# Compare to 10c, the train_error is a little smaller (which is consistent with the model being a little more flexible) 
# and the test_error is also a little smaller but still larger than the train_error


# 11a
set.seed(1)
n = 100
x1 = rnorm(n)
x2 = rnorm(n)
e = rnorm(n)
y = 5 + 10*x1 + 14*x2 + e

# 11b
b1_hat = 0

# 11c
a = y - b1_hat * x1
b2_hat = lm(a ~ x2)$coef[2]

# 11d
a = y - b2_hat * x2
b1_hat = lm(a ~ x1)$coef[2]

# 11e
b1_hat = 0
niters = 1000

b0_hats = rep(NA, niters)
b1_hats = rep(NA, niters)
b2_hats = rep(NA, niters)
for(i in 1:niters)
{
  a = y - b1_hat * x1
  b2_hat = lm(a ~ x2)$coef[2]
  
  a = y - b2_hat * x2
  b1_hat = lm(a ~ x1)$coef[2]
  
  b0_hats[i] = lm(a ~ x1)$coef[1]
  b1_hats[i] = b1_hat
  b2_hats[i] = b2_hat
}

par(mfrow=c(2,3))
plot(b0_hats, type="l", main="beta_0", log="x", xlab="# backfitting steps", ylab="beta_0")
plot(b1_hats, type="l", main="beta_1", log="x", xlab="# backfitting steps", ylab="beta_1")
plot(b2_hats, type="l", main="beta_2", log="x", xlab="# backfitting steps", ylab="beta_2")
# very fast convergence. This may be due to the fact that x1 and x2 are uncorrelated.
# let's try the data where x1 and x2 are correlated.

set.seed(1)
n = 100
x1 = rnorm(n)
x2 = x1 + x1^3 + rnorm(n)
e = rnorm(n)
y = 5 + 10*x1 + 14*x2 + e

b1_hat = 0
niters = 1000

b0_hats = rep(NA, niters)
b1_hats = rep(NA, niters)
b2_hats = rep(NA, niters)
for(i in 1:niters)
{
  a = y - b1_hat * x1
  b2_hat = lm(a ~ x2)$coef[2]
  
  a = y - b2_hat * x2
  b1_hat = lm(a ~ x1)$coef[2]
  
  b0_hats[i] = lm(a ~ x1)$coef[1]
  b1_hats[i] = b1_hat
  b2_hats[i] = b2_hat
}

plot(b0_hats, type="l", main="beta_0", log="x", xlab="# backfitting steps", ylab="beta_0")
plot(b1_hats, type="l", main="beta_1", log="x", xlab="# backfitting steps", ylab="beta_1")
plot(b2_hats, type="l", main="beta_2", log="x", xlab="# backfitting steps", ylab="beta_2")
# when x1 and x2 are correlated, the convergence is slower


# 11f
multip_fit = lm(y ~ x1 + x2)
multip_b0 = multip_fit$coef[1]
multip_b1 = multip_fit$coef[2]
multip_b2 = multip_fit$coef[3]

par(mfrow=c(1,3))
plot(b0_hats, type="l", main="beta_0", log="x", col="red", xlab="# backfitting steps", ylab="beta_0")
abline(multip_b0, 0, col="blue")
legend(5, 5.2, legend=c("backfitting", "multiple linear LS"), col=c("red", "blue"), lty=1)

plot(b1_hats, type="l", main="beta_1", log="x", col="red", xlab="# backfitting steps", ylab="beta_1")
abline(multip_b1, 0, col="blue")
legend(5, 4, legend=c("backfitting", "multiple linear LS"), col=c("red", "blue"), lty=1)

plot(b2_hats, type="l", main="beta_2", log="x", col="red", xlab="# backfitting steps", ylab="beta_2")
abline(multip_b2, 0, col="blue")
legend(5, 15.5, legend=c("backfitting", "multiple linear LS"), col=c("red", "blue"), lty=1)


# 11e
# We need only one step when x1 and x2 are uncorrelated and about 20 when x1 and x2 are correlated.


# 12
set.seed(1)
n = 1000
p = 100

x = matrix(rnorm(n*p), ncol=p)
true_betas = rnorm(p, mean=0, sd = 3)
true_beta0 = 5
y = true_beta0 + x %*% true_betas

backfitting = function(x, y, niters=10)
{
  n_predictors = ncol(x)
  est_betas = matrix(0, ncol=n_predictors, nrow=niters)
  est_beta0 = rep(0, niters)
  
  for(inter in 2:niters)
  {
    for(preditor_ind in 1:n_predictors)
    {
      tmp = y - x[, -preditor_ind] %*% est_betas[inter-1, -preditor_ind]
      est_betas[inter, preditor_ind] = lm(tmp ~ x[, preditor_ind])$coef[2]
    }
    est_beta0[inter] = lm(tmp ~ x[, preditor_ind])$coef[1]
  }
  return( list(est_beta0, est_betas) )
}

niters = 100
backfitting_results = backfitting(x, y, niters=niters)
est_beta0 = backfitting_results[[1]]
est_betas = backfitting_results[[2]]

multi_fit = lm(y ~ x)
multi_betas = multi_fit$coefficients[-1]
multi_beta0 = multi_fit$coefficients[1]

coef_converg_wrt_true = rep(0, niters)
coef_converg_wrt_multi = rep(0, niters)
for(iter in 1:niters)
{
  coef_converg_wrt_true[iter] = sqrt( (est_betas[iter,] - true_betas) %*% (est_betas[iter,] - true_betas) ) + 
    sqrt( (est_beta0[iter] - true_beta0)^2 )
  
  coef_converg_wrt_multi[iter] = sqrt( (est_betas[iter,] - multi_betas) %*% (est_betas[iter,] - multi_betas) ) +
    sqrt( (est_beta0[iter] - multi_beta0)^2 )
}

plot(coef_converg_wrt_true, xlab = "# backfitting steps", ylab="L2 norm of coefficients", col="red", type="l", log="x")
lines(coef_converg_wrt_multi, col="blue", type="l", log="x")
# converged after about 10 steps

