
# 8

college_datafile = "/Users/nthai/Learning/ISL_with_R/dataset/College.csv"
college = read.csv(college_datafile, header=TRUE)
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[, -1]
fix(college)
summary(college)
pairs(college[, 1:10])

plot(college$Private, college$Outstate, xlab="Private", ylab="Outstate")

# replicate "No" nrow(college) times, nrow() returns number of rows 
Elite=rep("No",nrow(college))
# if college having more than 50% in the Top10perc, assign Elite to "Yes"
Elite[college$Top10perc >50]="Yes"
# change Elite to qualitative with two levels Yes No 
Elite=as.factor(Elite)

# add Elite as a column to college
college=data.frame(college ,Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab="Elite", ylab="Outstate")

# divide the print window into four regions so that four plots can be made simultaneously. 
par(mfrow=c(2,2))

hist(college$Apps, breaks=20)
hist(college$Accept, breaks=20)
hist(college$Enroll, breaks=20)
hist(college$Top10perc, breaks=20)


# 9
auto_data_file = "/Users/nthai/Learning/ISL_with_R/dataset/Auto.csv"
Auto = read.csv(auto_data_file, header=TRUE, na.strings="?")
fix(Auto)
dim(Auto)

Auto = na.omit(Auto)
dim(Auto)

# use summary() is one way to know which are quantitative which are qualitative
# the origin should be qualitative
summary(Auto)

range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)


mean(Auto$mpg)
sd(Auto$mpg)

new_Auto = Auto[-seq(10, 85),]
mean(new_Auto$mpg)
sd(new_Auto$mpg)


pairs(Auto)

# heavier weight, less mpg
plot(Auto$mpg, Auto$weight)

# more horsepwer less mpg
plot(Auto$horsepower, Auto$mpg)

# more weight more horsepower
plot(Auto$weight, Auto$horsepower)

# cars become less heavier over time
plot(Auto$year, Auto$weight)

# cars become weaker over time
plot(Auto$year, Auto$horsepower)

# cars become more fuel efficient over time
plot(Auto$year, Auto$mpg)


# 10
library(MASS)
Boston
?Boston
dim(Boston)
names(Boston)

pairs(Boston)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$medv, Boston$crim)

