# 486 page
.libPaths('C:/Rlib')
library(car)
colnames(iris)
model = lm(Sepal.Length ~ Sepal.Width + Petal.Length+Petal.Width,data = iris)
sqrt(vif(model)) > 2

colnames(iris)
cor(iris[2:4])


# 모델에 사용할 데이터
# Sepal.Length ~ Sepal.Width + Petal.Length


# 데이터를 잘 섞어서 70%만 학습용으로 사용하고
# 나머지 30%는 테스트용으로 사용한다.

nrow(iris)
idx = sample(1:nrow(iris), nrow(iris)*0.7)
train = iris[idx,]
test =  iris[-idx,]
model2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length,data = train)


pred = predict(model2, test)
# 검증
# 분류문제면...  confusion matrix 이용해서 accuracy 또는 recall
# 계수를 보고 판단

cor(pred,test$Sepal.Length)

plot(pred,test$Sepal.Length)

#잔차(오차)
install.packages("lmtest")
library(lmtest)
dwtest(model2)

plot(model2, which = 1)


attributes( model2 )

res = residuals(model2)
shapiro.test(res)

sqrt(vif(model2)) > 2

summary(model2)
