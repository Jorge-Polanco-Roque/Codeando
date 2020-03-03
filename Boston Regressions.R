
### LAB ###

## 1)
credit <- read.csv('./data/Credit.csv', row.names=1)
head(credit)

md_full <- lm(Balance ~ Age + Rating + Limit, data=credit)
summary(md_full)

## 2)
adv <- read.csv('./data/Advertising.csv', row.names=1)
head(adv)

md_adv = lm(sales ~ ., data=adv)

summary(md_adv)

## 3)
auto <- fread('./data/Auto.csv')
auto$horsepower <- as.numeric(auto$horsepower)  # Correct data format
head(auto)
str(auto)

md_aut = lm(mpg ~ horsepower + I(horsepower^2), data = auto)
summary(md_aut)

md_auto = lm(mpg ~., data = auto)
summary(md_auto)

### Ex 2.2 ###

## a) 
# Regression / Inference
# n =500
# p = profit, number of employees, industry

## b)
# Classification / Prediction
# n = 20
# p = price charged, marketing budget, competition price, etc...

## c) 
# Regression / Prediction
# n = 52
# p = change in the US marke, change in the British market, change in the German market


### Ex 2.10 ###

## a) 
library(MASS)
data(Boston)
Boston = as.data.frame(Boston)
# 506 obs (obs for each town) of 14 variables

# b) 
pairs(Boston)
# relationship between crime rate per capita is not linear

# c) 
library(ggplot2)

boston_cor <- melt(Boston, id="crim")
ggplot(boston_cor, aes(x=value, y=crim)) + facet_wrap(~variable, scales="free") + geom_point()

high_cor = cor(Boston) > abs(.7) & cor(Boston) != 1

# d) 
crim = ggplot(Boston, aes(x=1:nrow(Boston), y=crim)) + geom_point()

tax = ggplot(Boston, aes(x=1:nrow(Boston), y=tax)) + geom_point()

ptr = ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio)) + geom_point()

# e)
sum(Boston$chas)

# f)
median(Boston$ptratio)

# g)
Boston[Boston$medv == min(Boston$medv),]
sapply(Boston, quantile)


# h)
nrow(Boston[Boston$rm > 7,])
nrow(Boston[Boston$rm > 8,])

rbind(sapply(Boston[Boston$rm > 8,], mean), sapply(Boston, median))

### Ex 3.3 ###

## a)
# Y = 50 + 20*GPA + 0.07*IQ + 35*Gender + 0.01*GPA:IQ - 10*GPA:Gender`

##b) 
#= 50 + 20x4.0 + 0.07x110 + 35x1 + 0.01x4.0x110 - 10x4.0x1

#= 137.1k USD

## c) 
# FALSE: IQ scale is larger than other predictors

### Ex 3.10 ##

## a)

require(ISLR)
data(Carseats)
fit.lm <- lm(Sales ~ Price + Urban + US, data=Carseats)
summary(fit.lm)

## b)
# Sales: sales in thousands at each location
# Price: price charged for car seats at each location
# Urban: No/Yes by location
# US: No/Yes by location


## c) 
# Sales = 13.043 - 0.054 x Price - 0.022 x UrbanYes + 1.201 x USYes

## d)
# Reject Null Hypothesis 

## e)
fit.lm1 <- lm(Sales ~ Price + US, data=Carseats)
summary(fit.lm1)

## f) 
# fit.lm (Price, Urban, US):
# RSE = 2.472
# R2 = 0.2393

# fit.lm1 (Price, US):
# RSE = 2.469
# R2 = 0.2393
    

## g)
confint(fit.lm1)

## h)
par(mfrow=c(2,2))
plot(fit.lm1)  
par(mfrow=c(1,1))
plot(predict(fit.lm1), rstudent(fit.lm1))
require(car)
qqPlot(fit.lm1, main="QQ Plot")
leveragePlots(fit.lm1)
plot(hatvalues(fit.lm1))

### Ex 3.15 ###

## a)
require(MASS)
data(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
names(Boston)[-1]
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") 
    stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
results <- combn(names(Boston), 2, 
                 function(x) { lmp(lm(Boston[, x])) }, 
                 simplify = FALSE)
vars <- combn(names(Boston), 2)
names(results) <- paste(vars[1,],vars[2,],sep="~")
results[1:13]

## b)
fit.lm <- lm(crim~., data=Boston)
summary(fit.lm)

## c)
results <- combn(names(Boston), 2, 
                 function(x) { coefficients(lm(Boston[, x])) }, 
                 simplify = FALSE)
(coef.uni <- unlist(results)[seq(2,26,2)])
(coef.multi <- coefficients(fit.lm)[-1])
plot(coef.uni, coef.multi)

## d) 
summary(lm(crim~poly(zn,3), data=Boston))      
summary(lm(crim~poly(indus,3), data=Boston))   
summary(lm(crim~poly(nox,3), data=Boston))     
summary(lm(crim~poly(rm,3), data=Boston))      
summary(lm(crim~poly(age,3), data=Boston))     
summary(lm(crim~poly(dis,3), data=Boston))     
summary(lm(crim~poly(rad,3), data=Boston))     
summary(lm(crim~poly(tax,3), data=Boston))     
summary(lm(crim~poly(ptratio,3), data=Boston)) 
summary(lm(crim~poly(black,3), data=Boston))   
summary(lm(crim~poly(lstat,3), data=Boston))   
summary(lm(crim~poly(medv,3), data=Boston))    
