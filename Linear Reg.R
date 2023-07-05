library(MASS)
library(ISLR)
plot(Boston$lstat,Boston$medv)
cor(Boston$lstat,Boston$medv)

lm.fit=lm(medv~lstat,Boston)
summary(lm.fit)

lm.fit2=lm(medv~lstat+I(lstat^2),Boston)
summary(lm.fit2)

medvp=predict(lm.fit2)
residuals(lm.fit2)

plot(Boston$lstat,Boston$medv)
lines(sort(Boston$lstat),fitted(lm.fit2)[order(Boston$lstat)],col="red")#

#multiple linear regression
lm.fit3=lm(medv~lstat+age,Boston)
summary(lm.fit3)

lm.fit4=lm(medv~.-age-indus+I(lstat^2),Boston)
summary(lm.fit4)

#age=a+b*lstat
#medv=b0+b1*lstat+b2*age=b0+b1*lstat+b2*(a+b*lstat)
#medv=b0+b1*lstat+b2*age+b3*(lstat*age)

lm.fit5=lm(medv~lstat+age+lstat:age,Boston)
summary(lm.fit5)

coef(lm.fit)
confint(lm.fit)

plot(lm.fit)
plot(lm.fit2)

num.cols <- sapply(Auto,is.numeric)
cor.data <- cor(Auto[,num.cols])
corrplot(cor.data)


#new data

x=c(34,108,64,88,99,51)
y=c(5,17,11,8,14,5)
ll=lm(y~x)
summary(ll)