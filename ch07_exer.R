

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
do.call(anova, polyno_reg_models)

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


# 10b
# this gives error
step_reg_model = glm(wage ~ cut(age, 2), data=Wage)
cv_fold = 10
cv.glm(Wage, step_reg_model, K=cv_fold)$delta[1]

# so what I am going to do?

