install.packages("randomForest")
library(randomForest)
data("iris")
str(iris)
model1 = randomForest(Species~.,data=iris)
model1

model2 = randomForest(Species~.,data=iris, ntree=300,mtry=4,na.rm = na.omit)
model2

model3 = randomForest(Species~.,data=iris, importance=T,na.rm = na.omit)
model3

importance(model3)


# 피처를 선택하는 방법
# 1. 문자와 날자를 제외하고...
# 2. lm()을 이용한 VIF 계수를 보고.. 이것을 바탕으로 상관계수 sqrt(VIF) > 2 제외
# 3. randomforest 를 이용해서 MeanDecreaseAccuracy 가 높은값은 기여도가 높음.. 낮은거 찾기
iris.df =  na.omit(iris)
summary(iris.df)
table(iris.df$Species)
iris.df$Species2[iris.df$Species == 'setosa'] = 1
iris$Species2[iris$Species == 'versicolor'] = 2
iris$Species2[iris$Species == 'virginica'] = 3
iris.df = iris.df[-5]
head(iris.df)
formula = Species2~.
model = lm(formula,data=iris.df)
sqrt(vif(model)) > 2
vif(model)

varImpPlot(model3)



#하이퍼 파라메터 튜닝