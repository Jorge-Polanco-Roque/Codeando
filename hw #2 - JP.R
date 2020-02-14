## Ex 6

# a)
v1 = exp(-6+0.05*40+1*3.5)/(1+exp(-6+0.05*40+1*3.5))
# .377

# b)
v2 = (log(0.5/(1-0.5)) + 6 - 3.5*1)/0.05
#50

## Ex 8
# There is not enough information to stablish which method is better.

## Ex 9

# a) 
v3 = 1/(1/0.37+1)
# Prob =  27%
  
# b)
v4 = 0.16/(1-0.16)
# Prob =  19%

## Ex 10

# a)
require(ISLR)
data(Weekly)
summary(Weekly)
pairs(Weekly)

# b)
fit_logit = glm(Direction~., data=Weekly[,c(2:7,9)], family=binomial)
summary(fit_logit)

# c)
lp = predict(fit_logit, Weekly, type="response")
lpred = ifelse(lp > 0.5, "Up", "Down")
table(lpred, Weekly$Direction)
(54+557)/nrow(Weekly)
# .56

## d) 
year_train = Weekly$Year %in% (1990:2008)
train = Weekly[year_train,]
test = Weekly[!year_train,]
fit2 = glm(Direction~Lag2, data=train, family=binomial)
fit2_prob = predict(fit2, test, type="response")
fit2_pred = ifelse(fit2_prob > 0.5, "Up", "Down")
table(fit2_pred, test$Direction)
mean(fit2_pred == test$Direction)
# .625

## e)
require(MASS)
fit_lda = lda(Direction~Lag2, data=train)
fit_lda_pred = predict(fit_lda, test)$class
table(fit_lda_pred, test$Direction)
mean(fit_lda_pred == test$Direction) 
# .625

## f)
fit_qda = qda(Direction~Lag2, data=train)
fit_qda_pred = predict(fit_qda, test)$class
table(fit_qda_pred, test$Direction)
mean(fit_qda_pred == test$Direction)
# .586

## g)
require(class)
set.seed(1)
train_X = as.matrix(train$Lag2)
test_X = as.matrix(test$Lag2)
knn_pred = knn(train_X, test_X, train$Direction, k=1)
table(knn_pred, test$Direction)
mean(knn_pred == test$Direction)
# .5

## h)
# Logistic and LDA present the best results

## i)
knn_pred = knn(train_X, test_X, train$Direction, k=5)
table(knn_pred, test$Direction)
mean(knn_pred == test$Direction)
knn_pred = knn(train_X, test_X, train$Direction, k=10)
table(knn_pred, test$Direction)
mean(knn_pred == test$Direction)
knn_pred = knn(train_X, test_X, train$Direction, k=20)
table(knn_pred, test$Direction)
mean(knn_pred == test$Direction)
knn_pred = knn(train_X, test_X, train$Direction, k=30)
table(knn_pred, test$Direction)
mean(knn_pred == test$Direction)
















