
#----------------------
# (8)
#---------------------
library(ISLR)
names(Auto)

# (a)
lm_mpg_on_hp = lm(mpg ~ horsepower, data=Auto)
summary(lm_mpg_on_hp)

# (i) since the p-value of F-statistic is very small, there is a relationship
# (ii) p-value very small so the relationship is very strong. R squared is ~ 60% 
# meaning about 60% of the variance has been explained by the model (good of fit).
# (ii) negative because the cofficient is negative, the slope goes down

# (iv) the center is the predicted value
predict(lm_mpg_on_hp, data.frame(horsepower=c(98)), interval="confidence", level = 0.95)
predict(lm_mpg_on_hp, data.frame(horsepower=c(98)), interval="prediction", level = 0.95)

# (b)
plot(Auto$horsepower, Auto$mpg)
abline(lm_mpg_on_hp)

# (c) diagnotic plot
par(mfrow=c(2,2))
plot(lm_mpg_on_hp)

# residual plot shows pronouced nonlinear relationship
# funel shape may mean the variance of random noice may not be constant -> heterocedestic
# There is point of high leverage


#-----------------------------
# (9)
#-----------------------------
#(a)
pairs(Auto)

# (b)
fix( cor(subset(Auto, select=-name)) )
lm_mpg_on_all_exept_name = lm(mpg ~ . -name, data=Auto)
summary(lm_mpg_on_all_exept_name)

# (i) p-value for F-statistic is very small -> there is a relationship
# (ii) displacement, weight, year, origin
# (iii) latter models give better mpg
    
# (d) diagnotic plot
par(mfrow=c(2,2))
plot(lm_mpg_on_all_exept_name)

# residual plot show some non-linear relationship
# points 327, 394 are outliers
# point 14 is high leverage 

# (e)
lm_mpg_on_sig_var = lm(mpg ~ displacement + weight + year + origin, data=Auto)
summary(lm_mpg_on_sig_var)

lm_mpg_on_sig_var_1 = lm(mpg ~ displacement + weight + year + origin + displacement*weight + year*origin, data=Auto)
summary(lm_mpg_on_sig_var_1)

lm_mpg_on_sig_var_2 = lm(mpg ~ displacement + weight + year + origin + weight*year + displacement*origin, data=Auto)
summary(lm_mpg_on_sig_var_2)

# (f)
# plot to check
sub_Auto = subset(Auto, select=c(mpg, displacement, weight, year, origin))
pairs(sub_Auto)
pairs(log(sub_Auto))

lm_mpg_on_sig_var_3 = lm(mpg ~ I(log(displacement)) + I(log(weight)) + year + origin, data=Auto)
summary(lm_mpg_on_sig_var_3)

# sqrt may be also good
pairs(sqrt(sub_Auto))
lm_mpg_on_sig_var_4 = lm(mpg ~ I(sqrt(displacement)) + I(sqrt(weight)) + year + origin, data=Auto)
summary(lm_mpg_on_sig_var_4)


#-------------------
# (10)
#-------------------
# (a)
library(ISLR)
names(Carseats)
lm.Sales.on.Price.Urban.US = lm(Sales ~ Price + Urban + US, data=Carseats)
summary(lm.Sales.on.Price.Urban.US)

# (b) coefficients
# Price: Average effect on Sales of increasing Price by one dollar.
# Small p-value indicates that there is a relationship between Price and Sales.
# Negative value indicates that higher Price is associated with lower Sales.

# UrbanYes: This coefficient give the average difference in Sales between Urban stores and rurual stores, 
# given that all other predictors are the same.
# Large p-value means that there is no relationship between Urban and Sales.

# USYes: This coefficient give the average difference in Sales between US stores and Non-US stores,
# given that all other predictors are the same.
# Small p-value means that there is a relationship between US and Sales.
# Possitive value means that on average, Stores in US sells more carseats than those in other countries, 
# given all other predictors are the same.

# (c)
# Sales =  13.043469 - 0.054459*Price - 0.021916*UrbanYes + 1.200573*USYes
# UrbanYes = 1 if Urban is Yes, UrbanYes = 0 otherwise
# USYes = 1 if US is Yes, USYes = 0 otherwise

# To see how qualitative predictors are encoded by R:
contrasts(Carseats$Urban)
contrasts(Carseats$US)

#(d) We can reject the null hypothesis (H_0 : beta_j = 0) for Price and USYes because their p-values are small

# (e)
lm.Sales.on.Price.US = lm(Sales ~ Price + US, data=Carseats)
summary(lm.Sales.on.Price.US)

# (f)
# lm.sales.on.Price.US has a slightly lower Residual standard error than lm.sales.on.Price.Urban.US
# Remember that RSE = sqrt( RSS / (n-p-1) ) (See Eq. 3.25).
# R-squared stays almost the same indicates that Urban has little effect on Sales.

# (g)
confint(lm.Sales.on.Price.US)


# (h)
par(mfrow=c(2,2))
plot(lm.Sales.on.Price.US)

# no outliers becuase standardized residuals are within -3 and 3.
# Review page 98 in the book: "The leverage statistic h_i is always between 1/n and 1, 
# and the average leverage for all the observations is always equal to (p+1)/n.
# So if a given observation has a leverage statistic that greatly exceeds (p+1)/n, 
# then we may suspect that the corresponding point has high leverage."
# In this case (p+1)/n = (2+1)/400 = 0.0075, and there are points significantly higher than this average.
# So the data may have some hight leverage issue.


#--------------------------------
# (11)
#--------------------------------
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
plot(x, y)

# (a)
lm.y.on.x.without.intercept = lm(y ~ x + 0)
summary(lm.y.on.x.without.intercept)
plot(x,y)
abline(lm.y.on.x.without.intercept)
# small standard error, hight t-statistic, very small p-value

# (b)
lm.x.on.y.without.intercept = lm(x ~ y + 0)
summary(lm.x.on.y.without.intercept)
plot(x,y)
abline(lm.x.on.y.without.intercept)
# similar conclusion for y as for x in (a)

# (c)
# lm.y.on.x.without.intercep in (a) provided a fit for the line y = 2*x + e. It gives the estimated coefficient
# as 1.9939 which is close to 2
# lm.x.on.y.without.intercep in (b) provided a fit for the same line, 
# but with x as a function of y: x = 0.5*y -0.5*e. 
# but it gives the estimated coefficient as 0.39111 which is a little bit off


# (d)
# See Figure_ch03_appl_exer_11d.pdf

# (e)
# The t-statistic for beta when for simple linear regression without intercept is symmetric with respect to
# interchanging x_i and y_i. 

# (f)
lm.y.on.x.with.intercept = lm(y ~ x)
summary(lm.y.on.x.with.intercept)
plot(x,y)
abline(lm.y.on.x.with.intercept)

lm.x.on.y.with.intercept = lm(x ~ y)
summary(lm.x.on.y.with.intercept)
plot(x,y)
abline(lm.x.on.y.with.intercept)


#---------------------------
# (12)
#--------------------------

# (a) When interchanging x and y in (3.38) leaves the estimate for beta the same, 
# i.e, when sum of squares of x is the same as sum of squares of y.

# (b)
# examples similar to 11a work
set.seed(1)
x = rnorm(100)
y = 5*x + rnorm(100)
plot(x, y)

lm.y.on.x.without.intercept = lm(y ~ x + 0)
summary(lm.y.on.x.without.intercept)

lm.x.on.y.without.intercept = lm(x ~ y + 0)
summary(lm.x.on.y.without.intercept)


# (c)
set.seed(1)
x = rnorm(100)
x = x[order(x)]
y1 = sample(x[1:50], 50, replace=FALSE)
y2 = sample(x[51:100], 50, replace=FALSE) 
y = c(y1, y2)
sum(x^2)
sum(y^2)
plot(x, y)

lm.y.on.x.without.intercept = lm(y ~ x + 0)
summary(lm.y.on.x.without.intercept) 
abline(lm.y.on.x.without.intercept)

lm.x.on.y.without.intercept = lm(x ~ y + 0)
summary(lm.x.on.y.without.intercept) 
abline(lm.x.on.y.without.intercept, col="red")

#--------------------------
# (13)
#-------------------------

# (a)
set.seed(1)
x = rnorm(100) 

# (b)
eps = rnorm(n=100, mean=0, sd=sqrt(0.25))

# (c)
y = -1 + 0.5*x + eps
length(y)
# beta_0 = -1, beta_1 = 0.5

# (d)
plot(x, y)
# there is a positive association between x and y 

# (e)
lm.y.onto.x = lm(y ~ x)
summary(lm.y.onto.x)

# p-value of F-statistic is very small -> there is a relationship between y and x
# p-values are very samll for both beta_0 and beta_1 -> they are statistically significant
# estimated for beta_0 and beta_1  are -1.01885 and 0.49947, respectively. They are close to
# the true values.

# (f)
abline(lm.y.onto.x, col="blue")
abline(a=-1, b=0.5, col="red")

legend("topleft", legend=c("linear reg. line", "population line"), col=c("blue", "red"), lwd=1)

# (g)
x2 = x^2
lm.y.onto.x.x2 = lm(y ~ x + x2)
summary(lm.y.onto.x.x2)

# No evidence that the quadratic term improves the model fit.
# estimate of coefficient of x2 is not statistically significant
# RSE just slightly decreases, and R-squared slightly increases.

plot(x, y)
abline(lm.y.onto.x, col="blue")
new_x = seq(-2.5, 2.5, length.out=20)
new_y = predict(lm.y.onto.x.x2, data.frame(x=new_x, x2=new_x*2))
lines(new_x, new_y, col="green")
abline(a=-1, b=0.5, col="red")
legend("topleft", legend=c("linear reg. line", "2nd polynomial reg. line", "population line"), col=c("blue", "green", "red"), lwd=1)


# (h)
# for later comparison
summary(lm.y.onto.x)

rm(list=ls())
set.seed(1)
x = rnorm(100)
# less noisy
eps = rnorm(n=100, mean=0, sd=sqrt(0.1))
y = -1 + 0.5*x + eps
lm.y.onto.x = lm(y ~ x)
summary(lm.y.onto.x)

# when data is less noisy, RSE decreases from 0.4814 to 0.3044.
# R-squared increases from 0.4674 to 0.687
# estimated coefficients are closer to the true values with more significant t-statistic
# F-statistic also increases.
# in summary, the linear model fits better when data are less noisy


# (i)
# more noise
rm(list=ls())
set.seed(1)
x = rnorm(100)
# less noisy
eps = rnorm(n=100, mean=0, sd=sqrt(0.5))
y = -1 + 0.5*x + eps
lm.y.onto.x = lm(y ~ x)
summary(lm.y.onto.x)

# opposite to 13h


# (j)
rm(list=ls())
set.seed(1)
x = rnorm(100)
eps = rnorm(n=100, mean=0, sd=sqrt(0.1))
y = -1 + 0.5*x + eps
confint(lm(y ~ x))

set.seed(1)
x = rnorm(100)
eps = rnorm(n=100, mean=0, sd=sqrt(0.25))
y = -1 + 0.5*x + eps
confint(lm(y ~ x))

set.seed(1)
x = rnorm(100)
eps = rnorm(n=100, mean=0, sd=sqrt(0.5))
y = -1 + 0.5*x + eps
confint(lm(y ~ x))

# they become wider and wider when the data is noisier and noisier.
 

#-------------------------
# (14)
#-------------------------
# (a)
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

# beta_0 = 2, beta_1 = 2, beta_2 = 0.3

# (b)
cor(x1,x2)
plot(x1, x2)

# (c)
lm.y.onto.x1.x2 = lm(y ~ x1 + x2)
summary(lm.y.onto.x1.x2)
confint(lm.y.onto.x1.x2)
# p-value for F-statistic is sigificant, there is a relationship between predictors and response
# estimated coefficients
# beta_0 = 2.1305  close to true beta_0 (2)
# beta_1 = 1.4396  off from true beta_1 (2)
# beta_2 = 1.0097  way off from true beta_2 (0.3)

# p-value for beta_1 is significant, we can reject the null hypothesis H_0: beta_1=0 
# p-value for beta_2 is not significant, so we can't reject the null hypothesis H_0: beta_2=0

# (d)
lm.y.onto.x1 = lm(y ~ x1)
summary(lm.y.onto.x1)
confint(lm.y.onto.x1)
# RSE and R-squares are almost the same.
# F-statistic increases -> stronger evidence of association  between predictor and response
# estimate of coefficient for beta_1 is much more accurate, loser to the true value and much samller SE.
# p-value of beta_1 becomes much more siginicant which provides stronger evidence againts the null hypothesis 
# H_0: beta_1=0

# (e)
lm.y.onto.x2 = lm(y ~ x2)
summary(lm.y.onto.x2)
confint(lm.y.onto.x2) 
# estimate of beta_2 (2.8996) is very different from the true value (0.3)
# p-value of beta_2 is very small which gives a strong evidence against the null hypothesis, H_0: beta_2=0

# (f). No the results in (c)-(e) do not contradict each other. 
# The phenomenon we saw in (c) is colinear problem. When predictors used in multiple linear
# regression are correlated, their effects on the response are difficult to separated.
# And their estimated coefficients have very large SE, and we may fail to reject the null hypothesis that
# the coefficients is zero.
# This colinear problem can be resoloved by removing one of the correlated predictors as what we did in (d) and (e).


# (g)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
plot(x1, x2)
# look like the new point is high-leverage point

lm.y.onto.x1.x2 = lm(y ~ x1 + x2)
summary(lm.y.onto.x1.x2)
confint(lm.y.onto.x1.x2)
par(mfrow=c(2,2))
plot(lm.y.onto.x1.x2)
# now the p-value of beta_1 becomes insignificant but the p-value of beta_2 becomes significant
# The SE of slopes' estimates reduce a little bit.
# this is due to the added high-leverage point.
# the added point is both outlier and high-leverage



#-----------------------------
# (15)
#-----------------------------
library(MASS)
summary(Boston)
?Boston
# chase: Charles River dummy variable, (= 1 if tract bounds river; 0 otherwise).
Boston$chas = factor(Boston$chas, labels = c("N","Y"))

predictors = c()
simple.lm.beta_j = c()

for (name in names(Boston)) {
    if (name != "crim") {
        print(paste("crim onto", name, "----------------------", sep=" "))
        x = Boston[,name]
        y = Boston[, "crim"]
        lm.fit = lm(y ~ x)
        model.summ = summary(lm.fit)
        print(model.summ)
        print(" ")
        print(" ")

        predictors = c(predictors, name)
        beta_j = lm.fit$coefficients[2]
        attributes(beta_j) = NULL
        simple.lm.beta_j = c(simple.lm.beta_j, beta_j)
        
}
}

# chas is not associated with crim

# (b)
lm.crim.onto.all = lm(crim ~ ., data=Boston)
summary(lm.crim.onto.all)

# p-value of F-statistic is small -> there is a relationship between predictors and response
# about 45% of the variance in the response was explained by the model.
# The predictors for which we can reject the null hypothesis H_0: beta_j = 0 are
# zn, dis, rad, black, medv

# (c)

multple.lm.beta_j = c()
for (name in predictors) {
    if (name == "chas") {name = paste(name, "Y", sep="")}
    beta_j = lm.crim.onto.all$coefficients[name]
    attributes(beta_j) = NULL
    multple.lm.beta_j = c(multple.lm.beta_j, beta_j)
}

plot(simple.lm.beta_j, multple.lm.beta_j)


# (d)
for (name in predictors) {
    print(paste("crim onto", name, "----------------------", sep=" "))
    x = Boston[,name]
    y = Boston[, "crim"]
    
    if (! is.factor(x) ) {
        lm.fit = lm(y ~ x + I(x^2) + I(x^3) )
        model.summ = summary(lm.fit)
        print(model.summ)
        print(" ")
        print(" ")
}
}

# predictors that have significant non-linear replationship whith crim are
# indus, nox, age, dis, ptratio, medv


