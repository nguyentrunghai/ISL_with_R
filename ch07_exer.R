

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



